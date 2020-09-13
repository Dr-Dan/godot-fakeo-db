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

func _run():
	ex_util.print_break()
	print("using a collection of objects (Dictionary)\n")
	open_operators()
	
# ==============================================================	
# DATA
# ==============================================================	

var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=16, addr_id=1, inv={money={coin=2000}, weapon={knife=10}, food={}, drink={relentless=1}}},
	{name="Carla", age=49, addr_id=2, inv={food={berry=20}}}]

var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250}]

# ==============================================================
# TODO: a breezy filter example + logic i.e. not, or, and, is etc
# ==============================================================

var id = 0
func open_operators():
	# get the value of a field in each item
	# this is only if open is called with a single String (not an Array or Dictionary)
	printt('names',
		fdb.mapply(name_table, ops.open('name')))
		
	# open multiple fields; use slashes to go deeper
	# result is a dictionary for each
	printt('view weapons, name and age',
		fdb.mapply(name_table, ['inv/weapon', 'name', 'age']))
		

	# use a dictionary allows mapping to fields that do not yet exist
	# 	fields in the array will be opened as before
#	var food_qry = ops.dict_apply({has_food='not _x.inv.food.empty()'}, ['name', 'inv/food'])
	printt('who has food?',
		fdb.mapply(name_table,
			{has_food='not _x.inv.food.empty()'}, ['name', 'inv/food']))
	# TODO: break into 2 args, move into filter example	
	# simple version in numbers?
	"""
	ops.comp will exit if ops.neq returns false after any step
	the last argument tells the comp' to return the value from the exit op on fail
		otherwise the last result is returned
	"""
	var op = ops.comp(
		[ops.open('inv/money/coin'), ops.gteq(10)],
		ops.neq(null))
			
	printt('who has coin >= 10?',
		fdb.qapply(name_table, fdb.qry()\
			.filter(op)\
			.map(['name', 'inv/money'])))
			
	# you can write a custom reducer
	printt('total knives:',
		fdb.reduce(
			fdb.iter(name_table, fdb.mapq(ops.open('inv/weapon/knife'))),
			Add.new()))

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
