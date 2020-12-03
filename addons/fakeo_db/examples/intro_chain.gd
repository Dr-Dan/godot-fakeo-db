tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)

the most common chainer functions take 3 arguments;
  static func xxx(input, coll=null, itbl=false): ...
	
	1 arg: returns a Transducer
	2 args: returns an Array
	3 args: returns an Iterable (if 'itbl' == true)


Unlike Operator/TransducerFactory, the chainer requires instantiating
However, it returns a copy of itself when any chaining function is called
  so changes will not affect other branches
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OpFactory


func _run():
	ex_util.pr_equals_break()
	args()
	ex_util.pr_equals_break()
	chaining()

var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="The Old One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}, food={nut=13}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=46, addr_id=1, inv={money={coin=2000}, weapon={knife=10, gun=2}, food={}, drink={relentless=1}}},
	{name="Carla", age=79, addr_id=2, inv={food={berry=20}}}]
	
func args():
	var chain = fdb.chain()
	# calling chain.xxx with 1 arg returns a Transducer
	var tx = chain.map(ops.open(['inv/weapon', 'name', 'age']))
	ex_util.pr_array('open fields', fdb.apply(tx, name_table))
		
	# calling chain.map with a String applies an expression
	ex_util.pr_array('deep age analysis',
		chain.map(
			'{"name":_x.name, "is_old":_x.age > 70}',
			name_table))

	# calling chain.xxx with the 'coll' (2nd) arg returns an Array
	ex_util.pr_array('view weapons, name and age',
		# chain.map([]) == chain.map(ops.open([])) == chain.project([])
		chain.map(
			['inv/weapon', 'name', 'age'],
			name_table))

	ex_util.pr_array('time-travel with some food',
		# if the first item in the Array is an op:
		#   chain.map([]) == chain.map(ops.comp([]))
		chain.map([
				ops.open(['name', 'age', 'inv/food']),
				ops.dict_apply({age='_x.age*10'}, ['name', 'food'])], 
			name_table))
			
	# and setting the last arg ('itbl') to true returns an Iterable for deferred evaluation
	var has_coin_itbl = chain.comp([
		# chain.filter({}) == chain.filter(ops.dict_cmpr({}))
		chain.filter({'inv/money/coin':ops.gteq(10)}),
		chain.map(['name', 'inv/money'])],
		name_table,
		true)

	# run iterable later
	ex_util.pr_array('who has coin >= 10?',
		has_coin_itbl.run())

	ex_util.pr_dash_break()
	# extract value then reduce
	printt('total knives:',
		chain.project('inv/weapon/knife')\
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


func chaining():
	var chain = fdb.chain()
	# chains can be branched off and extended later
	var map_mult_chn = chain.map('_x * 2')
	print(map_mult_chn.filter('_x >= 4', [1,2,3]))		
	print(map_mult_chn.skip(1).map('float(_x) / 2', [1,2,3]))
		
	var map_pow_chn = map_mult_chn.map('pow(_x, 3)')
	# chain -> Iterable
	printt('all:', map_pow_chn.itbl([1,2,3]).run())
	printt('2nd item:', map_pow_chn.itbl([1,2,3]).ffront())

	# bear in mind we are applying the chain from top to bottom
	#	to the data argument at the end
	print(chain\
		.filter(ops.is_var(TYPE_ARRAY))\
		.map(ops.open_idx('1/0'),
		['hello?', 22, {}, [0, [1]], [2], [3, [4]]]))

	# reduce requires evaluation so must be provided with a collection
	print(chain\
		.filter('_x > 3')\
		.map('_x * 2')\
		.reduce('_x + _y',
			range(10)))
		
