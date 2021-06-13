suppressMessages(library(readr))      # Read files
suppressMessages(library(tidyverse))  # All the tidy things
suppressMessages(library(dplyr))      # Data Cleaning
suppressMessages(library(dequer))     # Stacks

projectDir <- "~/dev/TransACT"
dateFormat <- "%m/%d/%Y"
lsSize <- 20
LSSize <- 40
options(width=200)

getDate <- function(prompt) {
  while (TRUE) {
    input <- as.Date(readline(prompt), dateFormat)
    if (!is.na(input)) {
      return(input)
    }
  }
}

getCategories <- function(categories) {
  mCategories <- with(categoryMappings, To[match(categories, From.Category)])
  categories <- ifelse(is.na(mCategories), categories, mCategories)
  categories <- ifelse(categories %in% knownCategories$Category, categories, sprintf("[%s]", categories))
  return(categories)
}

getAccounts <- function(accounts) {
  mAccounts <- with(accountMappings, To[match(accounts, From)])
  accounts <- ifelse(is.na(mAccounts), accounts, mAccounts)
  accounts <- ifelse(accounts %in% knownAccounts$Account, accounts, sprintf("[%s]", accounts))
  return(accounts)
}

extractMint <- function(mint) {
  earned <- ifelse(mint$Transaction.Type == 'credit', mint$Amount, "")
  paid <- ifelse(mint$Transaction.Type == 'debit', mint$Amount, "")
  
  #Categories
  categories <- getCategories(mint$Category)
  
  # Accounts
  accounts <- getAccounts(mint$Account.Name)
  destination <- ifelse(mint$Transaction.Type == 'credit', accounts, "")
  source <- ifelse(mint$Transaction.Type == 'debit', accounts, "")
  
  out <- mint %>% transmute(Description=Description, Date=as.Date(Date, dateFormat), Earned=as.double(earned), Paid=as.double(paid), Category=categories, Destination=destination, Source=source)
  out$Paid[is.na(out$Paid)] = 0
  out$Earned[is.na(out$Earned)] = 0
  return(out)
}

extractApple <- function(apple) {
  earned = ifelse(apple$Amount..USD. < 0, -1*apple$Amount..USD., "")
  paid = ifelse(apple$Amount..USD. > 0, apple$Amount..USD., "")
  
  #Categories
  categories <- getCategories(apple$Category)
  
  # Accounts
  destination <- ifelse(apple$Amount..USD. < 0, "Apple Card", "")
  source <- ifelse(apple$Amount..USD. > 0, "Apple Card", "")
  
  out <- apple %>% transmute(Description=Description, Date=as.Date(Transaction.Date, dateFormat), Earned=as.double(earned), Paid=as.double(paid), Category, Destination=destination, Source=source)
  out$Paid[is.na(out$Paid)] = 0
  out$Earned[is.na(out$Earned)] = 0
  return(out)
}

exportTrans <- function(trans) {
  trans$Earned <- str_replace_all(as.character(trans$Earned), "^0$", "")
  trans$Paid <- str_replace_all(as.character(trans$Paid), "^0$", "")
  write.csv(trans, sprintf("%s/out.csv",projectDir), row.names=FALSE)
}

safeGetFileName <- function(prompt, acceptBlank=FALSE) {
  while(TRUE) {
    filename <- readline(prompt)
    if (acceptBlank && filename == "" || file.exists(filename)) {
      return(filename)
    } else {
      message(sprintf("File \"%s\" doesn't exist, try again", filename))
    }
  }
}

categoryMappings <- read.csv(sprintf("%s/tables/category-mappings-mappings.csv", projectDir))
accountMappings <- read.csv(sprintf("%s/tables/account-mappings-mappings.csv", projectDir))
knownCategories <- read.csv(sprintf("%s/tables/categories-categories.csv", projectDir))
knownAccounts <- read.csv(sprintf("%s/tables/accounts-accounts.csv", projectDir))

loadNewTransactions <- function() {
  startDate <- getDate("Earliest Include Date: ")
  
  mint <- read.csv(safeGetFileName("Mint CSV: ")) %>% filter(as.Date(Date, dateFormat) >= startDate)
  combined <- extractMint(mint)
  
  while (TRUE) {
    file <- safeGetFileName("Apple CSV [enter when done]: ", acceptBlank = TRUE)
    if (file == "") {
      return(combined %>% arrange(combined$Date))
    } else {
      apple <- read.csv(file) %>% filter(as.Date(Transaction.Date, dateFormat) >= startDate)
      appleOut <- extractApple(apple)
      combined <- combined %>% rbind(appleOut)
    }
  }
  
}
saved <- sprintf("%s/out.csv", projectDir)
if(file.exists(saved) && readline(sprintf("Load Saved (%s) [y]/n: ", saved)) != "n") {
  transactions <<- read.csv(saved)
  transactions$Paid[is.na(transactions$Paid)] = 0
  transactions$Earned[is.na(transactions$Earned)] = 0
} else {
  transactions <<- loadNewTransactions()
}

undoStack <- stack()
push(undoStack, transactions)
exportTrans(transactions)

printContext <- function(i, length, vals, padding=TRUE) {
  if (padding) {
    cat("\n\n\n\n")
  }
  vals["Net"] = vals["Earned"]-vals["Paid"]
  vals["Description"] <- substr(vals$"Description", 0, 20)
  print(vals[i:(i+length-1),])
}


printHelp <- function() {
  message(        "h(elp):          Print help")
  message(        "<enter> | next:  Move to the next transaction")
  message(        "previous:        Move to the previou transaction")
  message(sprintf("ls:              Print %d transactions starting with the current one", lsSize))
  message(sprintf("LS:              Print %d transactions starting with the current one", LSSize))
  message(        "lscat:           List valid categories")
  message(        "lsaccount:       List valid accounts")
  message(        "d(escription):   Edit description")
  message(        "date:            Edit date")
  message(        "e(arned):        Edit amount earned")
  message(        "p(aid):          Edit amount paid")
  message(        "c(ategory):      Edit the category")
  message(        "destination:     Edit the source account")
  message(        "source:          Edit the source account")
  message(        "u(ndo):          Undo the last action")
}

doMerge <- function(trans, firstI, secondI) {
  first <- trans[firstI,]
  second <- trans[secondI,]
  destA <- first$Destination
  destB <- second$Destination
  srcA <- first$Source
  srcB <- second$Source
  
  if (destA != "" && destB != "" && srcA != "" && srcB != "") {
    if (destA == srcB && destB != srcA && first$Earned == second$Paid) {
      message(sprintf("Squashing %s account", destA))
      first$Destination <- second$Destination
      first$Earned <- second$Earned
    } else if (destB == srcA && destA != srcB && first$Paid == second$Earned) {
      message(sprintf("Squashing %s account", destB))
      first$Source <- second$Source
      first$Paid <- second$Paid
    }
  } else {
    if(!(destA==""||destB==""||destA==destB)) {
      message("Conflicting destinations, cannot merge")
      return(trans)
    }
    else if(!(srcA==""||srcB==""||srcA==srcB)) {
      message("Conflicting sources, cannot merge")
      return(trans)
    } else {
      message("Merging")
      first$Earned <- first$Earned + second$Earned
      first$Paid <- first$Paid + second$Paid
      if (first$Destination == "") { first$Destination <- second$Destination}
      if (first$Source == "") { first$Source <- second$Source}
    }
  }
  trans[firstI,] <- first
  trans <- deleteRow(trans, secondI)
  return(trans)
}

deleteRow <- function(df, i){
  rows <- dim(df)[1]
  df[i:(rows-1),] <- df[(i+1):rows,]
  df[-rows,]
}

executeCMD  <- function(cmd, trans) {
  if (cmd == "help" || cmd == "h") {
    printHelp()
  }
  else if (cmd == "ls") {
    printContext(i, lsSize, trans)
  }
  else if (cmd == "LS") {
    printContext(i, LSSize, trans)
  }
  else if (cmd == "lscat") {
    print(knownCategories$Category)
  }
  else if (cmd == "lsaccount") {
    print(knownAccounts$Account)
  }
  else if (cmd == "description" || cmd == "d") { 
    trans[i, "Description"] <- readline(sprintf("[%d] Description: \"%s\" --> ", i, trans[i, "Description"]))
  }
  else if (cmd == "date") {
    date <- try(as.Date(readline(sprintf("[%d] Date: %s --> ", i, format(trans[i, "Date"], format=dateFormat))), dateFormat), silent = TRUE)
    if (!is.na(date)){ trans[i, "Date"] <- date}
    else { message("Improperly formated date")}
  }
  else if (cmd == "earned" || cmd == "e") {
    earned <- try(as.double(readline(sprintf("[%d] Earned: $%0.2f\ --> $", i, trans[i, "Earned"]))), silent = TRUE)
    if (is.double(earned) && !is.na(earned)){ trans[i, "Earned"] <- earned}
    else { message("Improperly formated double")}
  }
  else if (cmd == "paid" || cmd == "p") {
    paid <- try(as.double(readline(sprintf("[%d] Paid: $%0.2f\ --> $", i, trans[i, "Paid"]))), silent = TRUE)
    if (is.double(paid) && !is.na(paid)){ trans[i, "Paid"] <- paid}
    else { message("Improperly formated double")}
  }
  else if (cmd == "category" || cmd == "cat" || cmd == "c") {
    category <- readline(sprintf("[%d] Category: \"%s\" --> ", i, trans[i, "Category"]))
    if (category %in% knownCategories$Category) {
      trans[i, "Category"] <- category
    } else {
      message("Unknown Category")
    }
  }
  else if (cmd == "destination" || cmd == "dest") {
    destination <- readline(sprintf("[%d] Destination: \"%s\" --> ", i, trans[i, "Destination"]))
    if (destination %in% knownAccounts$Account || destination == "") {
      trans[i, "Destination"] <- destination
    } else {
      message("Unknown Account")
    }
  }
  else if (cmd == "source" || cmd == "s") {
    source <- readline(sprintf("[%d] Source: \"%s\" --> ", i, trans[i, "Source"]))
    if (source %in% knownAccounts$Account || source == "") {
      trans[i, "Source"] <- source
    } else {
      message("Unknown Account")
    }
  }
  else if (cmd == "undo" || cmd == "u") {
    if (length(undoStack) > 1) {
      trans <- pop(undoStack)
      message("Undone")
    } else {
      message("Nothing to undo")
    }
  }
  else if (cmd == "merge" || cmd == "m") {
    second <- try(as.integer(readline(sprintf("Merge transaction #%d with transaciton #", i))), silent = TRUE)
    if (is.integer(second) && second > 0 && second <= dim(trans)[1]) {
      trans <- doMerge(trans, firstI=i, secondI = second)
    } else {
      message("Invalid transaction #")
    }
  } else if (cmd == "mergenext" || cmd == "mn") {
    if (i < dim(trans)[1]-1) {
      trans <- doMerge(trans, firstI=i, secondI = i+1)
    } else {
      message("There is no next transaction to merge with")
    }
  }
  
  else if (!is.na(str_match(cmd, "del(?:ete)?(?: (\\d+))?")[1])) {
    delIndex <- as.integer(str_match(cmd, "del(?:ete)? (\\d+)")[2])
    if (is.na(delIndex)) { delIndex <- i}
    printContext(delIndex, 1, trans, padding = FALSE)
    net <- as.double(trans[delIndex,"Earned"]-trans[delIndex,"Paid"])
    prompt <- sprintf("Confirm delete transaction #%d with net value %0.2f y/[n]: ",as.double(delIndex), net)
    if(delIndex == i || "y" == readline(prompt)) {
      trans <- deleteRow(trans, delIndex)
      message("Deleted")
    }
  } 
  else if (!is.na(str_match(cmd, "j(?:ump)? (\\d+)")[1])) {
    jumpindex <- as.integer(str_match(cmd, "j(?:ump)? (\\d+)")[2])
    if (is.integer(jumpindex) && jumpindex <= dim(trans)[1]) {
      i <<- jumpindex
    } else {
      message("Transaction number out of bounds")
    }
  }
  else {
    message(sprintf("Unknown Command: \"%s\"", cmd))
  }
  return(trans)
}



visitTransaction <- function() {
  vt <- transactions
  printContext(i, lsSize, vt)
  while (TRUE) {
    prompt <- sprintf("[%d]: ", i)
    cmd <- readline(prompt)
    if (cmd == "" | cmd == "next") {
      return(vt)
    } else if (cmd == "previous") {
      if (i > 1) {
        i <<- i-2
        return(vt)
      } else {
        message("No prior transaction")
      }
    } else if (cmd == "quit" || cmd == "q") {
      i <<- dim(vt)[1]+1
      return(vt)
    } else {
      new <- executeCMD(cmd, vt)
      if (dim(new) != dim(vt) || sum(new != vt) >= 1) { # value changed
        push(undoStack, new)
        vt <- new
        printContext(i, 1, vt, padding = FALSE)
        exportTrans(vt)
      } 
    }
  }
}

i <<- 1
while (i <= dim(transactions)[1]) {
  cat("\014")
  transactions <<- visitTransaction()
  i <<- i+1
}






