-- baseSelectFragment
SELECT
  items.itemID
, items.key
, items.dateAdded
, items.dateModified
, itemDataValues.value AS title
, tags
, creators
, attachmentItems.key AS attachmentKey
, itemAttachments.path AS attachmentPath

-- tagsJoinString
LEFT OUTER JOIN
(SELECT itemTags.itemID, GROUP_CONCAT(tags.name, ', ') AS tags
 FROM tags
 JOIN itemTags ON itemTags.tagID = tags.tagID
 GROUP BY itemTags.itemID)

-- creatorsJoinString
LEFT OUTER JOIN
(SELECT itemCreators.itemID, GROUP_CONCAT(creators.lastName, ', ') AS creators
 FROM itemCreators
 JOIN creators ON creators.creatorID = itemCreators.creatorID
 GROUP BY itemCreators.itemID)

-- tagsAndCreatorsAndFilenames
AND (   LOWER(tags)     = LOWER(?)
     OR LOWER(creators) LIKE LOWER(?)
     OR LOWER(itemAttachments.path) LIKE LOWER(?))
AND fields.fieldName = 'title'
LIMIT 20

-- baseQueryItemSelectFrom
{{&baseSelectFragment}}
--- extended property columns: see (zotero-get-special-properties-tables-selects zotero--special-properties-tables-query-plist)
--- , example
FROM items
--- get the entry title
JOIN itemData ON itemData.itemID = items.itemID
JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID
JOIN fields ON fields.fieldID = itemData.fieldID
--- extended property joins: see (zotero-get-special-properties-joins zotero--special-properties-tables-query-plist)
--- JOIN (SELECT itemTags.itemID , CASE WHEN tags.name LIKE 'example-%' THEN 'CUSTOM-EXAMPLE' ELSE   ' ' END AS example FROM tags JOIN itemTags ON itemTags.tagID = tags.tagID WHERE tags.name IN ('example-1', 'example-2') GROUP BY itemTags.itemID) AS exampleQ on exampleQ.itemID = items.itemID
{{&tagsJoinString}} AS tagsQ ON tagsQ.itemID = items.itemID
{{&creatorsJoinString}} AS creatorsQ on creatorsQ.itemID = items.itemID
--- get attachments
JOIN itemAttachments ON itemAttachments.parentItemID = items.itemID
JOIN items AS attachmentItems ON attachmentItems.itemID = itemAttachments.itemID
WHERE TRUE

-- queryByTitle
{{&baseSelectFragment}}
--- standard fields (title)
FROM items
JOIN itemData ON itemData.itemID = items.itemID
JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID
JOIN fields ON fields.fieldID = itemData.fieldID
{{&tagsJoinString}} AS tagsQ ON tagsQ.itemID = items.itemID
{{&creatorsJoinString}} AS creatorsQ on creatorsQ.itemID = items.itemID
--- attachments
JOIN itemAttachments ON itemAttachments.parentItemID = items.itemID
JOIN items AS attachmentItems ON attachmentItems.itemID = itemAttachments.itemID
WHERE 1
AND LOWER(itemDataValues.value) LIKE LOWER(?)
AND fields.fieldName = 'title'
GROUP BY items.itemID, fields.fieldName, itemAttachments.contentType
LIMIT 20

-- queryByAttributes
{{&baseSelectFragment}}
--- standard fields (title)
, ATTR_fields.fieldName
, ATTR_itemDataValues.value
FROM items
JOIN itemData ON itemData.itemID = items.itemID
JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID
JOIN fields ON fields.fieldID = itemData.fieldID
--- specific field
JOIN itemData AS ATTR_itemData ON ATTR_itemData.itemID = items.itemID
JOIN itemDataValues AS ATTR_itemDataValues ON ATTR_itemDataValues.valueID = ATTR_itemData.valueID
JOIN fields AS ATTR_fields ON ATTR_fields.fieldID = ATTR_itemData.fieldID
{{&tagsJoinString}} AS tagsQ ON tagsQ.itemID = items.itemID
{{&creatorsJoinString}} AS creatorsQ on creatorsQ.itemID = items.itemID
--- attachments
JOIN itemAttachments ON itemAttachments.parentItemID = items.itemID
JOIN items AS attachmentItems ON attachmentItems.itemID = itemAttachments.itemID
WHERE 1
AND (UPPER(?) = 'NULL' OR
     --- query-string
     ATTR_fields.fieldName = UPPER(?))
AND LOWER(ATTR_itemDataValues.value) LIKE LOWER(?)
AND fields.fieldName = 'title'
--- GROUP BY items.itemID, fields.fieldName
GROUP BY items.itemID, ATTR_fields.fieldName, itemAttachments.contentType
ORDER BY items.itemID ASC
LIMIT 20

-- queryByFulltextFragment
SELECT
--- NOTE, the target item is the PARENT, not the attachment item!
  parentItems.itemID
, parentItems.key, parentItems.dateAdded, parentItems.dateModified
, itemDataValues.value AS title
, tags
, creators
, attachmentItems.key AS attachmentKey
, itemAttachments.path AS attachmentPath
, fulltextWords.word
FROM fulltextWords
JOIN fulltextItemWords ON fulltextWords.wordID = fulltextItemWords.wordID
JOIN items AS attachmentItems ON fulltextItemWords.itemID = attachmentItems.itemID
JOIN itemTypes ON attachmentItems.itemTypeID = itemTypes.itemTypeID
--- get the parent item
JOIN itemAttachments ON itemAttachments.itemID = attachmentItems.itemID
JOIN items AS parentItems ON itemAttachments.parentItemID = parentItems.itemID
--- get the title of the PARENT item
JOIN itemData ON parentItems.itemID = itemData.itemID
JOIN itemDataValues ON itemData.valueID = itemDataValues.valueID
JOIN fields ON itemData.fieldID = fields.fieldID
{{&tagsJoinString}} AS tagsQ ON tagsQ.itemID = parentItems.itemID
{{&creatorsJoinString}} AS creatorsQ on creatorsQ.itemID = parentItems.itemID
WHERE 1
AND itemTypes.typeName = 'attachment'
AND fields.fieldName = 'title'
AND fulltextWords.word = LOWER(?)
GROUP BY parentItems.itemID
LIMIT 20
