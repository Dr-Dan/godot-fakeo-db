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
	print("using a collection of primitives (int)\n")
	primitive_intro()
	
	ex_util.print_break()
	print("using a collection of objects (Dictionary)\n")
	openings()
	
# ==============================================================	

func plus_y(x, y):
	return x+y

func multyz(x, y, z):
	return x*y*z

# used below in map
class Div:
	extends ops.Operators.OperatorBase
	var val
	
	func _init(val_=1):
		val = val_

	func eval(x):
		return float(x) / val
					
func primitive_intro():
	var data = range(10)
	print('data [x0, ..., x9]: ', data)
	
	var a = []
	for i in data:
		a.append(i + 2)
	printt('for-loop: x + 2', a)

	# mapply and fapply return arrays
	# f(ilter)apply
	# filter returns all in data where the op returns true
	printt('x is even (x%2==0)',
		fdb.fapply(data, ops.even()))
					
	# some ops contain others					
	printt('4 < x <= 9',
		fdb.fapply(data, ops.and_([ops.gt(4), ops.lteq(9)])))
						
						
	# m(ap)apply
	# map applies an op to each in data and returns the result
	# use an expression operator
	printt('x + 2',
		fdb.mapply(data, ops.expr('_x + 2')))
		
	# additional args passed to function
	printt('funcref: x + y',
		fdb.mapply(data, ops.func_(self, 'plus_y', [2])))
	
	# funcref converted to func op; args provided to mapply instead
	printt('x*y*z (ops.func_)',
		fdb.mapply(data, funcref(self, 'multyz'), [5, 10]))

	# use a class that derives from OperatorBase (^above)
	printt('user-op: x / 2',
		fdb.mapply(data, Div.new(2)))

	# call function from expr. string converted to expression op
	printt('x cubed',
		fdb.mapply(data, 'pow(_x, 3)'))

	# named args can be passed to an expression
	printt('pow(x, y)',
		fdb.mapply(data, 'pow(_x, y)', {y=10}))

	# comp passes each entry through a series of operators
	printt('x + 3 + y > 8',
		fdb.mapply(data,
			ops.comp([
				ops.expr('_x + 3'),
				ops.func_(self, 'plus_y', [2]),
				ops.gt(8)
				])))
				
				
	# reduce works with expressions, funcrefs, Operators.Expr, Operators.Func
	printt('add all items:',
		fdb.reduce(data, '_x + _y'))
		
	printt('multiply all items (except 0):',
		fdb.reduce(data.slice(1, -1), '_x * _y'))
		
# TODO: iterators example + count, front etc
# ==============================================================	
# ==============================================================	
# DATA
# ==============================================================	

var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="Anne", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=16, addr_id=1, inv={money={coin=2000}, weapon={knife=10}, food={}, drink={relentless=1}}},
	{name="Trish", age=49, addr_id=2, inv={food={berry=20}}}]

var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250}]

# ==============================================================	
# TODO: a breezy filter example + logic i.e. not, or, and, is etc
# ==============================================================	

var id = 0
func openings():
	# get the value of a field in each item
	# this is only if open is called with a single String (not an Array or Dictionary)
	printt('names',
		fdb.mapply(name_table, ops.open('name')))
		
	# open multiple fields; use slashes to go deeper
	printt('view weapons, name and age',
		fdb.mapply(name_table, ['inv/weapon', 'name', 'age']))
		
	# TODO: does not work for some reason
#	var food_qry = fdb.mapq({has_food='not _x.inv.food.empty()'}, ['name', 'inv/food'])
#	print(fdb.mapply(name_table, fdb.mapq(['name'])))

	# use a dictionary allows mapping to fields that do not yet exist
	# 	fields in the array will be opened as before
	printt('who has food?',
		fdb.mapply(name_table,
			{has_food='not _x.inv.food.empty()'}, ['name', 'inv/food']))
		
	# TODO: break into 2 args, move into filter example	
	# simple version in numbers?
	"""
	comp will exit if ops.neq returns false after any step
	the last argument tells the comp' to return the value from the exit op on fail
		otherwise the last result is returned
	"""
	var op = ops.comp(
		[ops.open('inv/money/coin'), ops.gteq(10)],
		ops.neq(null), true)
			
	printt('who has coin >= 10?',
		fdb.apply(name_table, fdb.qry()\
			.filter(op)\
			.map(['name', 'inv/money'])))
			
	# you can write a custom reducer also
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
