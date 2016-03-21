
Ideas and TODOs for MLgsc
=========================

Documentation
-------------

* **fix README.md**! it is a mess due to a merge.

`mlgsc_dump`
------------

* options to display only the tree [done] , or only one model (by ID) [done] ,
  or any other info that is or will be in the model [partly done]
* there seems to be a bug in DNA models (try it on `final_aln.bcls`).

`mlgsc_train`
-------------

* add command line used to create the model [done]
* add time stamp

`mlgsc`
-------

* alignment mode (-a) should subsume no-alignment (-A), i.e. alignment must be
  exactly one of global, semiglobal, or none. Keep -A for some time for
  compatibility
* (X)HTML/HTML5 output
* XML output for taxonomies, if it exists
* color output

`mlgsc_xval`
------------

* make sure all relevant options supported by `mlgsc` are also supported by
  `mlgsc_xval`, ao that their effect can be evaluated.
