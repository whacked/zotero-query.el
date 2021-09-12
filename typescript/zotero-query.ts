import * as Mustache from 'mustache'
import * as fs from 'fs'
import * as sqlite3 from 'sqlite3'


type YesqlMapping = Record<string, string>

function loadYesql(sqlSource: string): YesqlMapping {
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


const mapping = loadYesql(fs.readFileSync('../resources/sql/default.sql', 'utf-8'))


if (require.main == module) {
    // ts-node zotero-query.ts <search-word> <path-to-zotero-db>
    const db = new sqlite3.Database(process.argv[2])
    var stmt = db.prepare(mapping["queryByFulltextFragment"])
    stmt.each(process.argv[1], (err, row) => {
        console.log(row)
    }, () => {
        stmt.finalize()
    })
}