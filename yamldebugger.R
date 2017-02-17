## Check the quality of the quantlets with Lborke's yamldebugge:
  
  # Check for presence of the package:
  if(!(yamldebugger %in% rownames(installed.packages()))){
    devtools::install_github("lborke/yamldebugger")
  }

  library(yamldebugger)

  # Select one of the scripts embedded in the overall Quantlet folder
  maindir = dirname(dirname(file.choose()))

  # All-in-one function:
  yamlthis = function(qdir){
    d_init = yaml.debugger.init(qdir)
    qnames = yaml.debugger.get.qnames(d_init$RootPath)
    d_results = yaml.debugger.run(qnames, d_init)
    OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")
  }

  yamlthis(maindir)