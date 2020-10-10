tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)

AutoTransducers take 3 arguments
	static func map(input, coll=null, itbl=false): ...
	
	1 arg: returns a Transducer
	2 args: returns an Array
	3 args: returns an Iterable if 'itbl' == true
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OpFactory

# unlike Operator/TransducerFactory chain requires instantiating
#  upon calling map, filter it returns a copy of itself
#  so changes will not affect other branches
var flex = fdb.flex()

func _run():
	ex_util.pr_equals_break()
	print("using a collection of primitives (int)\n")
	auto_test()

var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="The Old One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}, food={nut=2}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=42, addr_id=1, inv={money={coin=2000}, weapon={knife=10, gun=2}, food={}, drink={relentless=1}}},
	{name="Carla", age=49, addr_id=2, inv={food={berry=20}}}]
	
func auto_test():
	# calling flex.xxx with 1 arg returns a Transducer
	var tx = flex.map(ops.open(['inv/weapon', 'name', 'age']))
	ex_util.pr_array('open fields', fdb.apply(tx, name_table))
		
	# calling flex.map with a String applies an expression
	ex_util.pr_array('deep age analysis',
		flex.map(
			'{"name":_x.name, "is_old":_x.age > 90}',
			name_table))

	# calling flex.xxx with the 'coll' (2nd) arg returns an Array
	ex_util.pr_array('view weapons, name and age',
		# flex.map([]) == flex.map(ops.open([])) == flex.project([])
		flex.map(
			['inv/weapon', 'name', 'age'],
			name_table))

	ex_util.pr_array('time-travel',
		# if the first item in the Array is an op:
		#   flex.map([]) == flex.map(ops.comp([]))
		flex.map([
				ops.open(['name', 'age', 'inv/food']),
				ops.dict_apply({age='_x.age*10'}, ['name', 'food'])], 
			name_table))
			
	# and setting the last arg ('itbl') to true returns an Iterable for deferred evaluation
	var has_coin_itbl = flex.comp([
		# flex.filter({}) == flex.filter(ops.dict_cmpr({}))
		flex.filter({'inv/money/coin':ops.gteq(10)}),
		flex.map(['name', 'inv/money'])],
		name_table,
		true)

	# run iterable later
	ex_util.pr_array('who has coin >= 10?',
		has_coin_itbl.run())

	ex_util.pr_dash_break()
	# extract value then reduce
	printt('total knives:',
		flex.project('inv/weapon/knife')\
		.reduce(
			Add.new(), 
			name_table))


# an operator for reducing should implement eval2(input_next, input_total)
class Add:
	extends ops.Operators.OperatorBase
	var val
	
	func _init(val_=0):
		val = val_

	func eval(x):
		return x + val
			
	func eval2(x_next, x_total):
		# if no item found, do nothing
		if x_total == null: return x_next
		return x_next + x_total
