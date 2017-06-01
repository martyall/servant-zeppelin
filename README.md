# servant-zeppelin
Server Side Loading JSON

The point of Servant-Zeppelin is to enable server side loading of JSON data without having to write boilerplate.
Take the following datatype for example:

```haskell
data Album =
  Album { albumId     :: AlbumId
        , albumName   :: String
        , albumOwner  :: PersonId
        , albumPhotos :: [PhotoId]
        }
```

It's often the case that we have such a datatype which carries foreign keys to other data, for example `PersonId` and `PhotoId`.
The client application would probably not be able to do anything useful with the JSON response for `Album` without making
additional requests to the server to fetch more data about the album owner or photos. We introduce a few new type classes
to capture what we call `Inflatable` data, meaning that there is a way to expand the data in some context. For example, if
the above `Album` is represented by a row in a postgres table, we probably already have functions laying around like

```haskell
getPersonById :: PersonId -> MonadPG Person
getPersonById = ...

getPhotosByIds :: [PhotoId] -> MonadPG [Photo]
getPhotosByIds = ...
```

We can use these functions to implement our `Inflatable` typeclass, e.g.

```
instance Infltable MonadPG PersonId where
  type Full MonadPG PersonId = Person
  inflator 
```

