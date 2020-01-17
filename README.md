# Fakeo DB

An attempt to replicate Linq/MongoDB behaviors in GDScript.

## Features

* Filter lists using custom queries
* Deferred execution
* Easy to extend operators and enumerables
* 

Note: At present only lists of dictionaries and objects are compatible i.e. no search on lists of primitives

## Quick Example

```gdscript
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = FakeoDB.OperatorFactory

# Combine objects and dictionaries
var data = [
	Weapon.new("Wooden Sword", "melee", "sword", 2, 1.2),
	...
	Weapon.new("Bone Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	...
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
]

# select all where subtype is 'bow' and dmg >=7. 
# Project to dictionary with {"name", "dmg", "atk_range"} as fields.
var query = fdb.list(data)\
	.where({subtype=ops.eq("bow"), dmg=ops.gteq(7)})\
	.project(["name", "dmg", "atk_range"])

func _run():
	print(query.to_array()) 
	# OR
  	for i in query: # in a for loop
		  print(i)
```

### And...

```gdscript
# this
var query = List.new(data).where(comparers).project(fields)
```

```gdscript
# is the same as
var qb = QueryBuilder.new().where(comparers).project(fields)
query = qb.eval(data)
```

```gdscript
# which is the same as
var list = List.new(data)
var where = Enumerables.Where.new(list, comparers)
var project = Enumerables.Project.new(where, fields)
```

### Also...

```gdscript
var query = fdb.list(data)\
	.where({subtype=ops.eq("bow"), dmg=ops.gteq(7)})
```

```gdscript
# is the same as
func _bow_condition(item):
	return item.subtype == "bow" and item.dmg >= 7

var query = fdb.list(data)\
	.where(funcref(self, "_bow_condition"))
```

## Overview
### [Database](../master/addons/fakeo_db/scripts/Database)
* Database - Stores collections

### [Enumerables](../master/addons/fakeo_db/scripts/Enumerables.gd)
* List - wrap a list with this to use where, project etc.
* Collection - an enumerable with append, erase functions and signals
* Where - get all satisfying a condition. Accepts functions and dictionaries.
* Project - project chosen fields into dictionary
* Take - take first N results
* Select - applies a function to each item and returns the result

* count(), at(index), first(pred), any(pred) can be called from enumerables
  * note that these will cause immediate evaluation
  * first and any both accept FuncRef and Dictionary types as predicates

### [Operators](../master/addons/fakeo_db/scripts/Operators.gd)

* HasField

* And
* Or

* Not

* Any
* None

* LT
* GT
* Eq

* In
* Contains

* CmpFunction
* CmpFunctionWithArgs


## Installation

1. Either download from the Godot Asset Store or just copy fakeo_db into the 'addons' folder in your project.

2. (Optional) Enable plugin: Project > Project Settings > Plugins

## Planned features

- [ ] Better examples
- [ ] ability to search any list i.e. ints, strings
- [ ] godot in-editor interface for database
- [ ] update functions on collections