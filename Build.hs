import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory

checkedBuildFlags :: FilePath -> String
checkedBuildFlags cwd =
    "--make -fhpc -O0 -i" ++ (cwd </> "src") ++ " -i" ++
    (cwd </> "test") ++ " -odir " ++
    (cwd </> "_build" </> "checked") ++ " -hidir " ++
    (cwd </> "_build" </> "checked")

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    want ["_build/release/xdraw" <.> exe]

    let releaseBuildFlags = "--make -O2 -isrc -odir _build/release -hidir _build/release"

    phony "clean" $ do
        removeFilesAfter "_build" ["//*"]

    --the release build config tool
    "_build/release/xdraw" <.> exe %> \out -> do
        sources <- getDirectoryFiles "" ["src//*.lhs"]
        need sources
        need ["_build" </> "checked" </> "test" </> "testMain" <.> exe]
        cmd "ghc" "-XFlexibleContexts" "-o" out releaseBuildFlags
                  ("src" </> "Main.lhs")

    --individual test executables
    "_build" </> "checked" </> "test" </> "testMain" <.> exe %> \out -> do
        cwd <- liftIO getCurrentDirectory
        let runDir = "_build" </> "checked" </> "test"
        sources <- getDirectoryFiles "" ["src//*.lhs"]
        need sources
        testSources <- getDirectoryFiles "" ["test//*.hs"]
        need testSources
        let hs = cwd </> "test" </> "Main" <.> "hs"
        () <- cmd (Cwd runDir) "ghc -o" ("testMain" <.> exe)
              (checkedBuildFlags cwd) hs
        () <- cmd (Cwd runDir) (cwd </> runDir </> "testMain" <.> exe)
        () <- cmd (Cwd runDir) "hpc" "report" "testMain"
        cmd (Cwd runDir) "hpc" "markup" "testMain"
