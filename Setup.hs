
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program ( runDbProgram )
import Distribution.Simple.Program.Types ( simpleProgram )
import Distribution.Simple.Setup

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { hookedPrograms    = [ simpleProgram "elm" ]
--    , postBuild         = myPostBuild
    }

{-
myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild _ bf _ lbi =
    runDbProgram (buildVerbose bf) (simpleProgram "elm") (withPrograms lbi)
        [ "make"
        , "--yes", "--warn"
        , "web-ui/src/Main.elm"
        , "--output", "files/htdocs/js/mdb.js"
        ]

myCopy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
myCopy pd lbi uh cf = do
    print $ dataFiles pd
    error "out"
    copyHook simpleUserHooks pd' lbi uh cf
    where
        pd' = pd
-}
