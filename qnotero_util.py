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

ZOTERO_FOLDER_PATH = glob(_p.expanduser('~/.*ozilla/*irefox/Profiles/*.default/zotero'))[0]

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


if __name__ == '__main__':

    if len(sys.argv) < 2:
        sys.exit()

    import json
    import base64

    output_klist = (
        'id', 'key',
        'authors', 'publication',
        'tags',
        'simple_format()',
        'fulltext',
    )
    query_string = sys.argv[1]
    res = zotero.search(query_string)
    json_str = (json.dumps([
        {k: (k.endswith('()')
             and getattr(it, k[:-2])()
             or getattr(it, k))
        for k in output_klist}
        for it in res
    ]))
    print(base64.b64encode(bytes(json_str, 'UTF-8')))
