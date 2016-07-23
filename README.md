---
title: Exhibitr
subtitle: Utility package for output generation
---

## Overview

**This is alpha software.  API is subject to change with future development**.

A simple library to help easily easily generate interactive Shiny modules from
code that produces static exhibits.

This doesn't deal with all types of interactivity; rather, it's intended to
provide a simple workflow to convert code that generates static exhibits to
interactive Shiny modules that support filtering.

The API is still under heavy development. Currently getting rid of the creation
of an intermediary "Shiny module" object so that the Shiny code will involve
only `xb_ui` (perhaps with post-processing functions to wrap them in tabs or
layout the components of an exhibit differently) and `xb_module` calls operating
directly on the exhibit object (still not sure if there are significant
performance penalties to some of this).

See `inst/examples/dashboard/app.R` for a usage example with comments.
`inst/examples/basic/app.R` is meant to be simpler, but is using an API that
will probably be phased out.

## Usage

1. Wrap up exhibit generation code in a function.
2. Create an `exhibitr` component for each element of an exhibit (e.g., one plot
   and one datatable). Currently supported are the following (though defining
   new objects here is pretty trivial, since they just need references to the
   Shiny output and render functions):
     - `xbc_plot` for plot and ggplot objects
     - `xbc_highchart` for highchart
     - `xbc_datatabe` for datatables
3. Create an `exhibitr` object that encapsulates those components.
4. ~~Create a Shiny module from the exhibit using `xb_shiny_module`.~~
5. Add UI components with `xb_ui` to the UI code
6. Add server logic with `xb_module`
