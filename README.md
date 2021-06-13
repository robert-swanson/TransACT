# Update Processes

1. Mint
   1. Go to [mint](https://mint.intuit.com/transaction.event) scroll to the bottom button that says “Export all <num> transactions”
   2. Copy name
   3. Update balances on spreadsheet
2.  Card
   1. Go to  Wallet >  Card > Card Balance > Relevant Statement > Export Transactions > CSV, AirDrop to Safron
   2. Copy name
   3. Update Balance on spreadsheet
3.  Run R and start TransACT to process transactions

```bash
R
source("~/dev/TransACT/TransACT.R")
```

4. Open  `~/dev/TransACT/out.csv` and copy to money records

