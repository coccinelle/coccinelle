import psycopg2
from datetime import date, datetime
from trac.util.datefmt import to_timestamp, utc

def add_ticket(self, dbinfo, summary, desc) :
        conn = psycopg2.connect(dbinfo)
        curs = conn.cursor()
        created = to_timestamp(datetime.now(utc))
        curs.execute("INSERT INTO ticket \
 (type,time,changetime,component,priority,owner,reporter,cc,version,milestone,status,summary,description, keywords) \
 VALUES \
 ('defect', %s, %s, 'other','major','somebody','Coccinelle','','next','','new','%s','%s', '')" % (created, created, summary, desc) )
        conn.commit()
