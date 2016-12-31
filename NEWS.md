# satchel 0.0.2

* update metadata to both have a json preview and serialize to an r_preview that retains attributes and class information
* metadata will naively handle types that are not convertable to JSON (eg ggplot objects) or objects that it
cannot introspect with a reasonable way of truncating (eg lists)
* `preview()` method implemented to show preview from stored metadata
* `auto_refresh(<bool>)` method implemented to make sure the satchel data tree is current when `use`-ing or `preview`ing data.
