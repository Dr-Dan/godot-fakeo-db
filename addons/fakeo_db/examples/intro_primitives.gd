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
	printt('x is even',
		fdb.fapply(ops.even(), data))
					
	# some ops can wrap others	
	# and_ takes a list of any size				
	printt('4 < x <= 9',
		fdb.fapply(ops.and_([ops.gt(4), ops.lteq(9)]), data))
							
	# m(ap)apply
	# map applies an op to each in data and returns the result
	# use an expression operator
	printt('x + 2',
		fdb.mapply(ops.expr('_x + 2'), data))
		
	# additional args passed to function
	printt('funcref: x + y',
		fdb.mapply(ops.func_(self, 'plus_y', [2]), data))
	
	# funcref converted to func op; args provided to mapply instead
	printt('x*y*z (ops.func_)',
		fdb.mapply(ops.func_(self, 'multyz', [5, 10]), data))

	# use a class that derives from OperatorBase (^above)
	printt('user-op: x / 2',
		fdb.mapply(Div.new(2), data))

	# call function from expr. string converted to expression op
	printt('x cubed',
		fdb.mapply(ops.expr('pow(_x, 3)'), data))

	# named args can be passed to an expression
	printt('pow(x, y)',
		fdb.mapply(ops.expr('pow(_x, y)', {y=10}), data))

	# comp(ose) passes each entry through a series of operators
	printt('(((x + 3) + y) > 8)',
		fdb.mapply(
			ops.comp([
				ops.expr('_x + 3'),
				ops.func_(self, 'plus_y', [2]),
				ops.gt(8)
				]), data))

	# ittr (iterator)
	printt('add all items:',
		fdb.ittr(ops.expr('_x + _y'), data).run())

	# reduce return the last result from ittr
	printt('multiply all items (except 0):',
		fdb.reduce(ops.expr('_x * _y'), data.slice(1, -1)))
		
# TODO: iterators example + count, front etc
# ==============================================================	
