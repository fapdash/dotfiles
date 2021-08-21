from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
from decimal import Decimal

class TeoConverter(CsvConverter):
    FIELDSET = set(["Buchungstag", "Wertstellungstag", "GegenIBAN", "Name Gegenkonto", "Verwendungszweck", "Umsatz", "Währung"])

    def __init__(self, *args, **kwargs):
        super(TeoConverter, self).__init__(*args, **kwargs)

    def convert(self, row):
        if row["Währung"] != "EUR":
            raise ValueError("Expected curreny to be 'EUR'", row["Währung"])
        posting_metadata = {"csvid": "teo.%s" % (self.get_csv_id(row))}
        amount = abs(Decimal(row['Umsatz'].replace(".", "").replace(",", ".")))
        reverse = row['Umsatz'].startswith('-')
        if reverse:
            account = 'Expenses'
        else:
            account = 'Income'
        if row['Name Gegenkonto']:
            payee = row['Name Gegenkonto']
        else:
            payee = "Umbuchung"

        return Transaction(
            date=datetime.datetime.strptime(row['Wertstellungstag'], "%Y-%m-%d"),
            payee=payee,
            postings=[Posting(self.name, Amount(amount, '€', reverse=reverse), metadata=posting_metadata),
                      Posting(account, Amount(amount, '€', reverse=not(reverse)), metadata=posting_metadata)],
            date_format=self.date_format,
        )
