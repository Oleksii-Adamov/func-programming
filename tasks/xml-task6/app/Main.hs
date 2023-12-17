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

createBooksXML :: IO ()
createBooksXML = do
  let bookstore = unode "bookstore" (map (Elem . bookToElement) books)
      doc = Element
              { elName = unqual "bookstore"
              , elAttribs = []
              , elContent = [Elem bookstore]
              , elLine = Nothing
              }
  writeFile "books.xml" (showElement doc)
  putStrLn "XML file 'books.xml' created successfully."

main :: IO ()
main = do
  createBooksXML
  putStrLn "XML created"