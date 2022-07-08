Guiding themes
----------------------

The goal of this repository is transparency. All co-authors should be able to see the project develop and its history. They should also be able to leave comments in the issue tracker and file pull requests to add/improve analysis and  text. Once submitted, the repository will serve as an open notebook to readers that are interested in better understanding methods so that they can critique our work and hopefully use our analysis in their own work. Whether this repository is public prior to submission is left to the first author; however, once submitted, the repository will be made public.

As each project is developed, the lead author should approach the project from the perspective a future user seeking to reproduce their work. At the basic level, the goal is for anyone to be able to clone the repository and run `make write.paper` to reproduce the analysis. There are many reasons why this may be hard or even impossible, but this should always be the goal. To achieve this it is important to keep in mind several factors:

* **Automation:** All analysis should be automated and executed using `make`. This will make sure that all file dependencies are maintained.

* **Organization:** All analysis should be run from the project's root directory without using `cd`. To achieve this, relative and not absolute paths should be used.

* **Software:** Document all external software, links, citations, and version numbers in the README document. Also document where your Makefile expects this software to be installed.

* **All code lives in the code folder:** Any batch or R scripts should be in the `code/` folder. We do this to create a partition between code and data. Tests should be kept in the `code/tests` folder.

* **Keep code DRY:** Code should make use of functions to eliminate repetition.

* **Automate tests as much as possible:** `testthat` is a powerful R package for performing unit tests to make sure your functions perform as expected.

* **Set the random number generator seed:** Use `set.seed` in mothur and R to set the random number generator so that your analysis is reproducible.

* **The heavy computational lifting should be in scripts:** Long computations should generate an output file that has a rule in the Makefile. The output of these computations can be used in building figures and tables and in the manuscript's Rmd file.

* **No hardcoding numbers in text:** All numbers from your study that appear in the final manuscript should have been generated using R code in the Rmd file. Use the R `stopifnot` function to make sure you are generating the expected outputs from your Rmd file (e.g. number of significant OTUs).

* **Raw data should stay raw:** All raw data should live in `data/raw/`. This includes metadata received from collaborators and fastq data taken from the sequencer. Data should only be manipulated using code and outputted to a separate folder (`data/processed/`).

* **Create tables in separate *.Rmd files:** The code and captions for tables should be stored in `results/tables/` and should be rendered to PDFs keeping the `TeX` formatted intermediate files.

* **Create figures in scripts:** The code for figures should be stored in `code/figures` and the output should be in the size and specifications set by the target journal.

* **Manuscript documents are stored in `documents/manuscript` folder:** Manuscripts should be in Miscrosoft Word format. 

* **Maintain as much of the exploratory phase as possible:** There is an exploratory folder for experimentation (`code/demo`) and maintaining notebooks (`code/notebook`) for situations where it is not possible to automate an analysis. Everything in the `code/scratch` directory can be deleted at any time without negative impact.

* **Keep all analysis under version control:** Commit your code after any significant changes to your project. Push the code to the GitHub repository at least daily.


See [Noble 2009] for a general description of and argument for the principle template structure. Some concepts and goals that guided this work.


Resources
---------

* [RMarkdown and knitr](http://rmarkdown.rstudio.com)
* [GNU Make](http://swcarpentry.github.io/make-novice/)
* [git](http://swcarpentry.github.io/git-novice/)



Acknowledgements
----------------

The initial file and directory structure of this project was developed by a group of participants in the Reproducible Science Curriculum Workshop, held at [NESCent] in December 2014 ([rr-init repository]). The structure is based on, and heavily follows the one proposed by [Noble 2009], with a few but small modifications. All copyright and related and neighboring rights to the original template were dedicated to the public domain worldwide under the [CC0 Public Domain Dedication]. The template and its derivatives are distributed without any warranty. It has been further modified by Dominick Lemas to fit the needs of his research group.

[rr-init repository]: https://github.com/Reproducible-Science-Curriculum/rr-init
[latest release]: https://github.com/Reproducible-Science-Curriculum/rr-init/releases/latest
[NESCent]: http://nescent.org
[Noble 2009]: http://dx.doi.org/10.1371/journal.pcbi.1000424
[CC0 Public Domain Dedication]: http://creativecommons.org/publicdomain/zero/1.0/
