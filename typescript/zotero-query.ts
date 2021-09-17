import * as Mustache from 'mustache'
import * as fs from 'fs'
import * as path from 'path'
import * as sqlite3 from 'sqlite3'
import { Database } from 'sqlite3'

export type YesqlMapping = Record<string, string>

export function loadYesql(sqlSource: string): YesqlMapping {
    let yesqlMapping = {}
    let buffer: Array<string> = []
    let flushBuffer = (label: string) => {
        let combinedFragment = buffer.join('\n')
        buffer = []
        yesqlMapping[label] = Mustache.render(combinedFragment, yesqlMapping)
    }
    let currentLabel: string
    sqlSource.split(/\r?\n+/g).forEach((line) => {
        let maybeLabel = /^--\s+(\S+)\s*$/.exec(line)
        if (maybeLabel != null) {
            if (currentLabel != null) {
                flushBuffer(currentLabel)
            }
            currentLabel = maybeLabel[1]
        } else {
            buffer.push(line)
        }
    })
    if (buffer.length > 0 && currentLabel != null) {
        flushBuffer(currentLabel)
    }
    return yesqlMapping
}

// TODO move this to external schema
export interface ZoteroItem {
    itemID: number  // 1234
    key: string // 'UZWT3Q7C',
    dateAdded: string // '2013-07-05 16:48:14',
    dateModified: string // '2013-07-05 16:48:14',
    title: string // 'dorsal plastic finicker',
    tags: string // 'car, boat, animal',
    creators: string // 'Fuu, Boo',
    attachmentKey: string // 'AANGQ32N',
    attachmentPath: string // 'storage:dorso-plastic-finicker.pdf',
    fieldName?: string
    [key: string]: any
}

async function executeStatement(db: Database, preparedStatementTemplate: string, ...statementParameters: Array<string>): Promise<Array<ZoteroItem>> {
    var preparedStatement = db.prepare(preparedStatementTemplate)
    return new Promise((resolve, reject) => {
        let rows: Array<ZoteroItem> = []
        try {
            preparedStatement.each(...statementParameters, (err, row) => {
                rows.push(row)
            }, () => {
                preparedStatement.finalize()
                resolve(rows)
            })
        } catch (e) {
            reject(e)
        }
    })
}

export async function queryByTitle(db: Database, titleString: string): Promise<Array<ZoteroItem>> {
    return executeStatement(db, sqlMapping["queryByTitle"], `%${titleString}%`)
}

export async function queryByAttribute(db: Database, attributeString: string): Promise<Array<ZoteroItem>> {
    return executeStatement(db, sqlMapping["queryByAttributes"], 'NULL', 'NULL', `%${attributeString}%`)
}

export async function queryByFulltextFragment(db: Database, searchString: string): Promise<Array<ZoteroItem>> {
    return executeStatement(db, sqlMapping["queryByFulltextFragment"], searchString)
}

export const sqlMapping = loadYesql(fs.readFileSync(
    path.join(
        path.dirname(__filename),
        '../resources/sql/default.sql'), 'utf-8'))


if (require.main == module) {
    // ts-node zotero-query.ts <search-word> <path-to-zotero-db>
    const db = new sqlite3.Database(process.argv[3])
    queryByTitle(db, process.argv[2]).then((rows) => {
        rows.forEach((row) => {
            console.log(row)
        })
    })
}
