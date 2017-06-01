# servant-zeppelin
Server Side Loading JSON

## Overview
The point of servant-zeppelin is to enable server side loading of JSON data without having to write boilerplate.
Take the following `Album` datatype for example, which is expanded in greater detail in the tests:

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
getPersonById :: PersonId -> PGMonad Person
getPersonById = ...

getPhotosByIds :: [PhotoId] -> PGMonad [Photo]
getPhotosByIds = ...
```

We can use these functions to implement our `Inflatable` typeclass, e.g.

```haskell
instance Infltable PGMonad PersonId where
  type Full MonadPG PersonId = Person
  inflator = getPersonById 
```

and similarly for `[PhotoId]`. We can then indicate that `Album` has dependencies on these datatypes like this:

```haskell
instance HasDependencies Album '[PersonId, [PhotoId]] where
  getDependencies album = albumOwner album &: albumPhotos album &: NilDeps
```

This gives us access to a new servant combinator `SideLoad (deps :: [*])` which can be used at the end of a typed route in the following way:

```haskell
...
  :<|> Capture "album" AlbumId :> Get '[PlainText, JSON] Album :> SideLoad '[Person, [Photo]]
...
```

The semantics are similar to `QueryFlag`-- the the presence of the keyword `sideload` in the query params, or the key value pair `sideload=true` or `sideload=1`, will trigger a response with the additional sideloaded data if the desired content type is `application/json`. The absence of this flag returns the normal `JSON` serialization. If it was `PlainText` in this example, nothing out of the ordinary happens.

Here is an example of the different responses:


```json
{
  "albumId": 1,
  "albumPhotos": [
    1,
    2
  ],
  "albumName": "Vacations",
  "albumOwner": 1
}
```

```json
{
  "data": {
    "albumId": 1,
    "albumPhotos": [
      1,
      2
    ],
    "albumName": "Vacations",
    "albumOwner": 1
  },
  "dependencies": {
    "person": {
      "personName": "Alice",
      "personId": 1
    },
    "photos": [
      {
        "artistId": 1,
        "photoCaption": "At the Beach.",
        "photoId": 1
      },
      {
        "artistId": 1,
        "photoCaption": "At the Mountain.",
        "photoId": 2
      }
    ]
  }
}
```

## servant-zeppelin-server
Much of what was needed to understand the server component was explained above. In order to get the `ToJSON` instances for your side loaded data, it's sufficient to have `ToJSON` instance for all the components and that the components of the dependencies are instances of a type family called `NamedDependency`. More concretely, in the above example we would have needed
```haskell
instance ToJSON Person
type instance NamedDependency = "person"

instance ToJSON [Photo]
type instance NamedDependency = "photos"

instance ToJSON Album
```

in order to derive and instance `ToJSON (SideLoaded Album '[Person, [Photo]])`, which is sufficient to support the route. 

The second component which was needed is a way to transfer the context of the inflation to servant's `Handler` monad. Concretely, if our monad `PGMonad` above was newtyped around something like `ReaderT Connection (ExceptT QueryError IO)`, we need to provide a natural transformation of type `PGMonad :~> Handler` to the `Context` when we define our application. In principle it might happen that you use different contexts for different datatypes if you were for example maintaining two seprate databases. This is ok as long as you provide both transformations to the `Context`. You cn see the tests for more details.

## servant-zeppelin-swagger
In order to have the swagger docs generate for an endpoing using the `:> SideLoad deps` combinator, we need to have a `ToSchema` instance for `SideLoad a deps`. The will be automatically derived for you with sensible choices proveded that you have a `ToSchema` instance for both `a` and every `d` in `deps`. I find that with swagger, a picture is worth more than code, but you can see the tests for how to generate this:

![Route](https://github.com/martyall/servant-zeppelin/blob/master/images/Route.png?raw=true)
![Model](https://github.com/martyall/servant-zeppelin/blob/master/images/Model.png?raw=true)

## servant-zeppelin-client
We also provide a `HasClient` instance for the `SideLoad deps` combinator, though it behaves a little bit differenty than most `HasClient` instances you might have run into so far. The problem is that a real `HasClient` instance is dependently typed-- if you give me `sideload=true` param, I exepect the sideloaded data `SideLoad a deps`, and if it's not there I expect something of type `a`. In order to get around the type systems limitations, the `HasClient` instance provides us with a dependently typed function which then can be given singlton values to get the desired type. 

Also, the client has a bias for using requesting `JSON`-- this seems fair to me because there is yet no reason to use a `SideLoad` combinator on a route without `JSON` as a valid mime type. The necessary `FromJSON` instances are supplied in the client lib as well, and as usually they can be automatically derived so long as the components all have instances.

The client library also exposes a typeclass `ProjectDependency` implementing a single method `projectDependency`. This has the same semantics as servant's `HasContext` typeclass, and can be useful to get data out of a side loaded response. 
Again, see the tests for examples.
