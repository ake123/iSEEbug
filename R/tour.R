.landing_page_tour <- rbind(
    data.frame(
        element="#import\\.panel",
        intro=paste(
          "First, you need to import a dataset from files. Several formats are",
          "supported. Alternatively, you can experiment on miaDash with one of",
          "the ready-made <a href='https://microbiome.github.io/mia/reference/mia-datasets.html'>mia datasets</a>.",
          "<br/><br/>When ready, click on the Upload button to import the",
          "dataset as a TreeSummarizedExperiment object."
        )
    ),
    data.frame(
        element="#manipulate\\.panel",
        intro=paste(
            "You may need to wrangle the data a little bit to get the best",
            "visualisations out of it. Here, you can apply operations such as",
            "subsetting, agglomeration and transformation to the dataset.",
            "<br/><br/>After setting the proper parameters, click on the Apply",
            "button to perform the operation on the dataset."
        )
    ),
    data.frame(
        element="#estimate\\.panel",
        intro=paste(
            "Here, you can perform standard microbiome analysis to estimate",
            "alpha (within-sample) and beta (between-sample) diversity with",
            "several methods.<br/><br/>Click on the Compute button to apply",
            "selected method to the dataset."
        )
    ),
    data.frame(
        element="#visualise\\.panel",
        intro=paste(
            "Here you can select which panels to use to visualise the dataset.",
            "Check the <a href='https://bioconductor.org/packages/devel/bioc/vignettes/iSEEtree/inst/doc/iSEEtree.html#12_Panels'>iSEEtree catalogue</a>",
            "to find the right panel.<br/><br/>When ready, click the Launch",
            "button to explore the dataset."
        )
    ),
    data.frame(
        element="#output\\.panel",
        intro=paste(
            "Here you see the TreeSummarizedExperiment object you created.",
            "Every time you press the Upload, Apply or Compute buttons, new",
            "elements are added to the object.<br/><br/>You can download the",
            "object as an RDS file by clicking the Download button."
        )
    )
)