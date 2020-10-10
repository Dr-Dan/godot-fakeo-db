# Fakeo DB

For Godot 3.2

An addon that makes querying arrays easy.

## Features

* Works with lists of objects and primitives
* Simple, functional, compositional style
    * composible operators and transducers
* Optional deferred execution using Iterable

## About
### [Wiki](../../wiki)

## Quick Example
```gdscript
# an array of int
var numbers = range(10)

# get first 3 even numbers
var even = fdb.flex().filter(ops.even()).take(3).apply(numbers)
# multiplied by 2
var multiplied = fdb.flex().map('_x * 2', numbers)
# sum all
var sum = fdb.flex().reduce('_x + _y', numbers)

var scores = [
    {name='da-best', email='db@gmail.com', region='EU', score=190520, hours_played=2400},
    {name='wizz', email='wizzl@aol.com', region='US', score=40200, hours_played=230},
    ...]

var score_per_hour = fdb.flex()\
    .map(ops.dict_apply({sph='_x.score/_x.hours_played'}, ['name']), 
    scores)
    
var high_scores = fdb.flex()\
    .filter({region='EU'})\
    .map(['name', 'score'])\
    .sort('_x.score > _y.score', 
    scores)

```
TODO: links to other examples

## Installation

1. Either download from the Godot Asset Store or just copy fakeo_db into the 'addons' folder in your project.

## Sorry
This addon is not production tested and will probably be slower than handwritten code i.e. for loops. 

## License

Provided under the [MIT license](../master/LICENSE)
