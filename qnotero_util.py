'''
Python3 only

zotero interaction using libzotero by Qnotero

ref https://github.com/smathot/qnotero
ref http://www.cogsci.nl/blog/tutorials/97-writing-a-command-line-zotero-client-in-9-lines-of-code
(which is an older version using Gnotero, now renamed Qnotero)

'''
import sys
if sys.version_info[0] < 3:
    print('Python3 required')
    sys.exit()
sys.path.insert(0, '/opt/qnotero')

import os.path as _p
from glob import glob
from libzotero import libzotero

# what happens to windows?
ZOTERO_FOLDER_PATH = glob(_p.expanduser('~/.*ozilla/*irefox/*rofiles/*.default/zotero'))[0]

zotero = libzotero.LibZotero(ZOTERO_FOLDER_PATH)

# examples:
# zotero.search(query) returns a list of libzotero.zotero_item.zoteroItem objects.
# item_list = zotero.search('LISP')
# query is a (str), and by default matches against everything, including:
# title, author, tag, publication...
# you can specify which attribute to match on e.g.
# item_list = zotero.search('author:Shannon')
# item_list = zotero.search('publication:PLOS')
# combine them with
# item_list = zotero.search('publication:Science tag:memory')
#
# another example:
# res = zotero.search('fMRI')
# for i, item in enumerate(res):
#     print('-'*40)
#     print('{} of {}'.format(i, len(res)))
#     print(item.simple_format())
#     # interesting attributes, e.g.
#     # item.id
#     # item.title
#     # item.tags # list of <str>
#     # item.fulltext # path to local file

def search_by_doi(doi_string):
    cur = zotero.conn.cursor()
    doi_fieldID = cur.execute("SELECT fieldID FROM fields WHERE fieldName LIKE 'DOI' LIMIT 1").fetchone()[0]
    out = []
    for row in cur.execute("SELECT idata.itemID FROM itemData AS idata, itemDataValues AS idv " +
                      "WHERE idata.fieldID = ? " +
                      "AND idata.valueID = idv.valueID " +
                      "AND idv.value LIKE ?",
                      (doi_fieldID, doi_string)).fetchall():
        out.append(row[0])
    cur.close()
    return out

def postprocess(dres_list):
    cur = zotero.conn.cursor()
    doi_fieldID = cur.execute("SELECT fieldID FROM fields WHERE fieldName LIKE 'DOI' LIMIT 1").fetchone()[0]
    for dres in dres_list:
        row = cur.execute("SELECT idv.value FROM itemData AS idata, itemDataValues AS idv " +
                          "WHERE ? = idata.itemID " +
                          "AND idata.fieldID = ? " +
                          "AND idata.valueID = idv.valueID",
                          (dres['id'], doi_fieldID)).fetchone()
        if row:
            dres['doi'] = row[0]
    cur.close()

if __name__ == '__main__':

    if len(sys.argv) < 2:
        sys.exit()

    import json
    import base64

    query_string = sys.argv[1]
    if query_string.lower().startswith('doi:'):
        doi_string = query_string[4:]
        print('searching for: %s' % doi_string)
        match_id_list = search_by_doi(doi_string)
        res = [zotero.index[z_id] for z_id in match_id_list]
    else:
        res = zotero.search(query_string)

    # convert to dict list
    output_klist = (
        'id', 'key',
        'authors', 'publication',
        'title', 'tags',
        'simple_format()',
        'fulltext',
    )
    dres_list = [{(k.endswith('()') and k[:-2] or k):
                  (k.endswith('()') and getattr(it, k[:-2])() or getattr(it, k))
                  for k in output_klist} for it in res]

    # add extra fields if possible
    postprocess(dres_list)
    
    if '--no-base64' not in sys.argv:
        json_str = json.dumps(dres_list)
        print(base64.b64encode(bytes(json_str, 'utf-8')).decode('utf-8'))
    else:
        print(json.dumps(dres_list, indent=2))
        
