from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
from decimal import Decimal

class DkbConverter(CsvConverter):
    FIELDSET = set(["Buchungsdatum", "Wertstellung", "Status", "Zahlungspflichtige*r", "Zahlungsempfänger*in", "Verwendungszweck", "Umsatztyp", "IBAN", "Betrag (€)", "Gläubiger-ID", "Mandatsreferenz", "Kundenreferenz"])

    def __init__(self, *args, **kwargs):
        super(DkbConverter, self).__init__(*args, **kwargs)

    def convert(self, row):
        posting_metadata = {"csvid": "dkb.%s" % (self.get_csv_id(row))}
        amount = abs(Decimal(row['Betrag (€)'].replace(".", "").replace(",", ".")))
        reverse = row['Betrag (€)'].startswith('-')
        if reverse:
            account = 'Expenses'
        else:
            account = 'Income'

        if row['Zahlungspflichtige*r'] and not reverse:
            payee = row['Zahlungspflichtige*r']
        elif row['Zahlungsempfänger*in']:
            payee = row['Zahlungsempfänger*in']
        else:
            payee = "Umbuchung"

        cleared = row['Status'].__eq__('Gebucht')
        date = row['Wertstellung'] if cleared else row['Buchungsdatum']

        if not date or not cleared:
            # Ignore unsettled transactions and transactions without any dates
            return None

        posting_from = Posting(self.name, Amount(amount, '€', reverse=reverse), metadata=posting_metadata)
        posting_to = Posting(account, Amount(amount, '€', reverse=not(reverse)), metadata=posting_metadata)

        return Transaction(
            # date is supposed to be 'Buchungsdatum', aux_date is 'Wertstellung'
            # but I ignore non-cleared transactions anyways, so date=aux_date
            date=datetime.datetime.strptime(date, "%d.%m.%y"),
            payee=payee,
            postings=[posting_from,
                      posting_to],
            date_format=self.date_format,
            cleared=cleared
        )
