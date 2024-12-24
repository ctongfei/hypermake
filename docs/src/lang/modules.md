# Module System

### Objects

In HyperMake, sometimes it is needed to bundle certain definitions together so that it can be reused. 
This forms an **object** in HyperMake (think of it as a singleton object in OO languages):

```python
object my_obj:
  key = value
  def f(...):
    ...
  
  task t0(...) -> out = f(...)
```

Objects can be used as namespaces. To refer to a definition in an object, use the `.` sign. For example, to refer to
 - the `key` in `my_obj`, write `$my_obj.key`；
 - the `out` output in task `t0`, write `$my_obj.t0.out`.

> Given a task `a.b.c`, it will be placed in `${fileSys.root}/a/b/c`.


### Classes

**Classes** are just abstract objects that can be instantiated with parameters.
```python
class my_class(param0, param1, ...):
  key = value
  task t0(...) -> out:
    ...
```
To instantiate a class, write
```python
object my_obj = my_class(arg0, arg1, ...)
```
> Note that instantiation of an object starts with the keyword `object`. 
> Just doing `my_obj = ...` would define a string-valued literal.

### Modules
HyperMake's module system is based on objects: each file, when imported, forms a singleton object.
Given the following directory structure:
 - `main.hm`  
 - `data/`
   - `preprocess.hm`

In `main.hm`, you can import `preprocess.hm` as follows:
```python
import data.preprocess
```
This will create an object `data.preprocess` that contains all definitions in `preprocess.hm`. Or you can import it as an alias:
```python
import data.preprocess as pp
```
This will create an object `pp` that contains all definitions in `preprocess.hm`.

Additionally, you can import a HyperMake script in the current namespace, not as an object:
```python
import "data/preprocess.hm"
```
Importing a file by its filename will import all definitions in `preprocess.hm` into the current namespace.
