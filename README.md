# from-text [![Hackage](http://img.shields.io/hackage/v/from-text.svg)](https://hackage.haskell.org/package/from-text) [![Stackage LTS](http://stackage.org/package/from-text/badge/lts)](http://stackage.org/lts/package/from-text) [![Stackage Nightly](http://stackage.org/package/from-text/badge/nightly)](http://stackage.org/nightly/package/from-text)

This package provides

```haskell
class IsText a where
  fromText :: Text -> a
```

aiming to simplify conversion from `Text` to other textual data types, including `ByteArray`, `ByteString` and `OsPath`. When converting to binary types without an associated encoding, we use UTF-8.

There is an overwhelming number of alternative packages for text conversions. To name a few in no particular order:
* [`text-conversions`](https://hackage.haskell.org/package/text-conversions-0.3.1.1/docs/Data-Text-Conversions.html)
* [`string-conv`](https://hackage.haskell.org/package/string-conv-0.2.0/docs/Data-String-Conv.html)
* [`ttc`](https://hackage-content.haskell.org/package/ttc-1.5.0.1/docs/Data-TTC.html)
* [`text-convert`](https://hackage-content.haskell.org/package/text-convert-0.1.0.1/docs/Text-Convert.html)
* [`lawful-conversions`](https://hackage-content.haskell.org/package/lawful-conversions-0.4.0/docs/LawfulConversions.html)
* [`witch`](https://hackage-content.haskell.org/package/witch-1.3.1.0/docs/Witch-Instances.html)
* [`unwitch`](https://hackage-content.haskell.org/package/unwitch-3.0.0/docs/Unwitch-Convert-Text.html)

At the moment none of them provide conversions from `Text` to `OsPath`. I could not decide which one to contribute such function to, so decided to create a new package.
