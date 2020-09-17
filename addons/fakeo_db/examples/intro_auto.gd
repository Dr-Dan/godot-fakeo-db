tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory
const auto = fdb.AutoProcessors

func _run():
	ex_util.print_break()
	print("using a collection of primitives (int)\n")
	auto_test()

var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="The Old One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}, food={nut=2}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=42, addr_id=1, inv={money={coin=2000}, weapon={knife=10}, food={}, drink={relentless=1}}},
	{name="Carla", age=49, addr_id=2, inv={food={berry=20}}}]
	
func auto_test():
	# calling auto.xxx with the 'coll' argument as null returns a Processor
	var tx = auto.map(ops.open(['inv/weapon', 'name', 'age']))
	for i in fdb.apply(tx, name_table):
		print(i)
		
	ex_util.print_break_mini()
		
	# calling auto.xxx with a collection returns an Array
	printt('view weapons, name and age',
		auto.map(
			ops.open(['inv/weapon', 'name', 'age']),
			name_table))
			
	ex_util.print_break_mini()

	var comp_op = ops.comp(
		[ops.open('inv/money/coin'), ops.gteq(10)],
		ops.neq(null))

	# and setting 'itbl' to true returns an Iterable for deferred evaluation
	var has_coin_itbl = auto.comp([
		auto.filter(comp_op),
		auto.map(ops.open(['name', 'inv/money']))],
		name_table, 
		true)
	printt('who has coin >= 10?',
		has_coin_itbl.run())
		
	ex_util.print_break_mini()

	var ittr_tx = auto.ittr(Add.new())
	var knives = auto.map(
		ops.open('inv/weapon/knife'),
		name_table, true)
		
	printt('total knives:',
		fdb.apply(
			ittr_tx,
			knives))
			
	ex_util.print_break_mini()

	# enumerate is the exception to the rule as it has 2 args before 'coll'
	print(
		auto.filter(
			ops.run('idx', ops.gteq(2)), 
			auto.map(
				ops.open(['name', 'idx', 'age']), 
				auto.enumerate('idx', 1, name_table))))

# an operator for reducing should implement eval2(input_next, input_accumulated)
class Add:
	extends ops.Operators.OperatorBase
	var val
	
	func _init(val_=0):
		val = val_

	func eval(x):
		return x + val
			
	func eval2(x_next, x_total):
		# if no item found, do nothing
		if x_next == null: return x_total
		return x_next + x_total
