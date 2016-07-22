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

## Usage

1. Wrap up exhibit generation code in a function.
2. Create an `exhibitr` component for each element of an exhibit (e.g., one plot
   and one datatable).
3. Create an `exhibitr` object that encapsulates those components.
4. Create a Shiny module from the exhibit using `xb_shiny_module`.
