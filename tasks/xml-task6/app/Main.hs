import Text.XML.Light

data Book = Book
  { category :: String
  , title :: String
  , author :: String
  , year :: String
  }

books :: [Book]
books =
  [ Book "Fantasy" "The Way of Kings" "Brandon Sanderson" "2010"
  , Book "Science Fiction" "Dune" "Frank Herbert" "1965"
  ]

bookToElement :: Book -> Element
bookToElement (Book cat t a y) =
  unode "book" $
    [ unode "category" cat
    , unode "title" t
    , unode "author" a
    , unode "year" y
    ]

createBooksXMLWithDTD :: IO ()
createBooksXMLWithDTD = do
  let bookstore = unode "bookstore" (map (Elem . bookToElement) books)
      doc = Element
              { elName = unqual "bookstore"
              , elAttribs = []
              , elContent = [Elem bookstore]
              , elLine = Nothing
              }
      dtdContent =
              "<!ELEMENT bookstore (book+)>" ++
              "<!ELEMENT book (category, title, author, year)>" ++
              "<!ELEMENT category (#PCDATA)>" ++
              "<!ELEMENT title (#PCDATA)>" ++
              "<!ELEMENT author (#PCDATA)>" ++
              "<!ELEMENT year (#PCDATA)>"

  writeFile "books.xml" $ unlines ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                    ,"<!DOCTYPE bookstore SYSTEM \"bookstore.dtd\">"
                                    ,showElement doc]

  writeFile "bookstore.dtd" dtdContent

  putStrLn "XML file 'books.xml' with DTD 'bookstore.dtd' created successfully."

main :: IO ()
main = do
  createBooksXMLWithDTD