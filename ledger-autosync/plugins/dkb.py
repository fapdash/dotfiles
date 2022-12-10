from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
from decimal import Decimal

class TeoConverter(CsvConverter):
    FIELDSET = set(["Buchungstag", "Wertstellung", "Buchungstext", "Auftraggeber / Begünstigter", "Verwendungszweck", "Kontonummer", "BLZ", "Betrag (EUR)", "Gläubiger-ID", "Mandatsreferenz", "Kundenreferenz"])

    def __init__(self, *args, **kwargs):
        super(TeoConverter, self).__init__(*args, **kwargs)

    def convert(self, row):
        posting_metadata = {"csvid": "dkb.%s" % (self.get_csv_id(row))}
        amount = abs(Decimal(row['Betrag (EUR)'].replace(".", "").replace(",", ".")))
        reverse = row['Betrag (EUR)'].startswith('-')
        if reverse:
            account = 'Expenses'
        else:
            account = 'Income'
        if row['Auftraggeber / Begünstigter']:
            payee = row['Auftraggeber / Begünstigter']
        else:
            payee = "Umbuchung"

        return Transaction(
            date=datetime.datetime.strptime(row['Wertstellung'], "%d.%m.%Y"),
            payee=payee,
            postings=[Posting(self.name, Amount(amount, '€', reverse=reverse), metadata=posting_metadata),
                      Posting(account, Amount(amount, '€', reverse=not(reverse)), metadata=posting_metadata)],
            date_format=self.date_format,
        )
