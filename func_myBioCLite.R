# Don't install packages that are up-to-date

myBioCLite <- function() {
     suggestedVersions <- old.packages()
     installedVersions <- installed.packages()[ rownames(suggestedVersions),"Version" ]
     suggestedVersions <- suggestedVersions[,"ReposVer"]
     needToInstall <- suggestedVersions!=installedVersions
     if (sum(needToInstall) > 0) {
          installThese <- names(suggestedVersions)[which(needToInstall)]
          cat("\ncalling biocLite with this list of packages\n",installThese,"\n")
          biocLite(installThese, suppressUpdates=TRUE, suppressAutoUpdate=TRUE)
     } else {
          cat("\nThere are no new packages to install\n")
     }
     cat("\ndone\n\n")
}