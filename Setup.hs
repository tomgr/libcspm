import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath (replaceExtension)
                        
main = defaultMainWithHooks
	simpleUserHooks {hookedPreProcessors=[("ppy", transformation)]}

transformation :: BuildInfo -> LocalBuildInfo -> PreProcessor
transformation bi lbi = 
	PreProcessor {
		platformIndependent = True,
		runPreProcessor =
			mkSimplePreProcessor $ \inFile outFile verbosity -> do
				let midFile = replaceExtension outFile "y"
				runSimplePreProcessor (ppCpp' ["-optP-P"] bi lbi) inFile midFile verbosity
				runSimplePreProcessor (ppHappy bi lbi) midFile outFile verbosity
	}
