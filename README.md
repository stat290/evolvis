evolvis
-------

**evolvis** is **evol**ution **vis**ualization

The goal of evolvis is a generalized toolset for generating lineage of some composition, such as a document, from a series of events such as revisions, and visualizing the lineage interactively.

The evolvis package provides three categories of functions which work together: \* XML transform \* Text differencing \* Interactive visualization

### XML transform:

Transforms XML documents into a series of text revisions. For the initial use case these are WikiMedia documents, which is the XML schema for Wikipedia pages. When connected to the internet, XML documents can be downloaded on demand.

### Text differencing:

The `evolution` function is the main entry point, and returns an object of type `evolution`. It uses formula syntax to express the evolution of differences in the response variable `y` as a function of the version `v(...)` by content class, for example `texts ~ v(revisionNum) + author`. `diff.fun` is required and specifies the differencing function.

### Interactive visualization:

The plot.evolution function is an S3 dispatch method for the internal generic plot function. It requires the `ggvis` package and produces a `ggvis` object which can be rendered using the generic `print` method or `ggvis::view_dynamic` or `ggvis::view_static`.

### Background:

The package was inspired by the IBM Research *History Flow* project <https://www.research.ibm.com/visual/projects/history_flow/> for visualizing the history and composition of Wikipedia articles.

<!-- README.md is generated from README.Rmd. Please edit that file -->
