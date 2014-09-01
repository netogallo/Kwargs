## Kwargs

This is a packet that provides a very easy way to create command line argument parsers using convention over configuration. It does not intend to replace packages like **cmdargs** but it's scope is quick prototyping of applications. If you are, for example, in the early development of an application, Kwargs allows you to easily create a command line argument parser which you can later replace with something more powerful like cmdargs. With all honesty, I wrote this to improve the quality of homework assignments w/o having to do much.

### Usage

1) Define your settings type, which derives from generic. Make sure the 'DeriveGeneric' compiler flag is turned on:

```
{-# Language DeriveGeneric -#}
import GHC.Generics

data Config = Config{
    o1 :: String, -- Regular string specified by --o1=value
    o2 :: Bool, -- Flag which is set to True when --o2 is passed and set to False when not passed
    o3 :: Maybe String, -- Same as option1 but it is set to Nothing when --o3 is not present
    o4a_o4b :: Maybe (String, String)  -- It is set to Nothing when neither flag is present, 
                                       -- set to the value 'Just (val1,val2) when --o4a=val1 and --o4b=val2 is present
                                       -- Fails if only one of the two flags is present
} deriving (Generic, Show)
```

2) Read the argumetns:

```
   import System.Console.CmdArgs.Generic (kwargs, getBuilders)
   import Control.Applicative ((<$>)) -- For syntactic convenience

   main = (kwargs (getBuilders :: BaseBuilder Config) <$> getArgs) >>= print
```

3) Assuming that the code from step 2 is compiled into an executable called 'args':

```
> args --o1=v1 --o2 --o3=v2 --o4a=v3 --o4b=v4
Right (Config{o1="v1", o2=True, o3=Just "v3", o4=Just ("v3","v4")})
> args --o1=v1 --o4a=v3 --o4b=v4
Right (Config{o1="v1", o2=False, o3=Nothing, o4=Just ("v3","v4")})
> args --o2 --o3=v2 --o4a=v3 --o4b=v4
Left ("--o1 is missing")
```