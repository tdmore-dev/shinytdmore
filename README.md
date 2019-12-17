# This project
`shinytdmore` provides a series of UI elements to easily build dose adaptation software.

The project focuses on four things:

1. Flexible input elements to easily define the patient covariates, treatment history and observations.
2. Clear graphs to show the input, a priori parameter distributions, the estimation result, and future predicted concentrations vs target.
3. Clear graphs to show the dose search space, the evaluation criteria and the selected dose.
4. Reporting elements to show the recommended dose and what will happen if we apply this dose.

# License
This project does not include a license. This means that all work is under exclusive copyright. Nobody else can use, copy, distribute or modify this work.

The Github terms of service apply. We allow others to view and fork the repository. Please note that this is not sufficient to then copy, distribute or modify this work further.

Please see https://choosealicense.com/no-permission/ for more information.

Through publishing, we allow others to use this R package and to perform dose adaptation. Installing this package using `devtools::install_github` is allowed.

The official copyright holder of this work is the KU Leuven university.

## Dependencies and aggeregate derivative work
Please note this package has several dependencies. As soon as shinytdmore is distributed in aggregate with these dependencies (e.g. as an Android application), you have to abide by all software license terms.

Many of these dependencies are GPL-licensed, and are therefore viral: as soon as you include these packages to make a derivative work, [the whole system should be distributed under the terms of the GPL](https://www.gnu.org/licenses/gpl-faq.en.html#IfLibraryIsGPL).

> Q: If a library is released under the GPL (not the LGPL), does that mean that any software which uses it has to be under the GPL or a GPL-compatible license?

> A: Yes, because the software as it is actually run includes the library.

The following discussion on the `dplyr` issue [#2260](https://github.com/tidyverse/dplyr/issues/2260#issuecomment-266834064) summarizes the issue very well. The package itself can be distributed under any license terms, but the combination with its dependencies should be distributed under GPL.

# Limitations
This software is a research project, and cannot be considered as a medical device. It is not a substitute for clinical reasoning.
