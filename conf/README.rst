====================================
``conf`` -- Core Emacs Configuration
====================================

The ``conf`` directory holds a couple of core configuration files that get
loaded during Emacs startup, and hold all of the core functionality. They are,
in order of loading: 

- ``local-functions`` holds all functions that used to be scattered around my
  configuration that I haven't managed to attach to specific packages, or are
  necessary for loading other parts of the configuration.
  
- ``settings`` holds all site wide configuration that is not associated with a
  package.

- ``programming`` holds all ``use-package`` directives, which contain all
  configuration related to specific packages, and optimized for delayed
  loading and fast startup times.
