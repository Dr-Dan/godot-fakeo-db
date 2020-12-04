# Fakeo DB

Written in Godot 3.2

An addon that makes querying arrays of objects and/or primitives easy.

## Usage

I will write some documentation but for now I hope the examples will suffice.

[examples...](../master/addons/fakeo_db/examples)

```gdscript
var numbers = range(5)
var chain = Fakeo.chain()

chain.filter(ops.even(), numbers) 
# => [0,2,4]
chain.filter([ops.gt(1), ops.lteq(4)], numbers) 
# => [2,3,4]

chain.map('_x * 5_', numbers) 
# => [0,5,10,15,20]

chain.sort('_x > _y', data)
# => [4,3,2,1,0]

chain\
    .filter(ops.is_var(TYPE_ARRAY))\
    .map(ops.open_idx('1/0'),
        ['hello?', 22, {}, [0, [1]], [2], [3, [4]]])) 
# => [1,4]


var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}}},
	{name="The Old One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=10}, weapon={knife=2}}},
    ]

chain.map(ops.open('name'), name_table)
# => [{name="Mike"}, ...]

chain.filter({'inv/money/coin'=ops.gt(5)}).map(['name', 'inv/money/coin'], name_table)
# => [{name="Mike", coin=25}, {name="Anne", coin=10}]

chain.project('inv/weapon/knife')\
    .reduce('_x + _y', name_table)
# => 4
```

## Installation

1. Either download from the Godot Asset Store or just copy fakeo_db into the 'addons' folder in your project.


## Sorry
This addon is not production tested and will probably be slower than handwritten code i.e. for loops. 

It is also likely to change dramatically up to v1.0.

## License

Provided under the [MIT license](../master/LICENSE)
