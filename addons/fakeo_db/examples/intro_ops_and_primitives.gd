tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)

"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OpFactory

func _run():
	ex_util.pr_equals_break()
	print("using a collection of primitives (int)\n")
	operators()
	
# ==============================================================	

# used in funcref
func less_than_6(x):
	return x < 6

func plus_xy(x, y):
	return x+y

func mult_xyz(x, y, z):
	return x*y*z

class Div:
	extends ops.Operators.OperatorBase
	var val
	
	func _init(val_=1):
		val = val_

	func eval(x):
		return float(x) / val
					
func operators():
	var data = range(10)

	var filter_ops = [
		{
			msg='x is even', 
			op=ops.even()
		},
		{
			# can use ops.odd() operator but for demonstration...
			msg='x is odd', 
			op=ops.not_(ops.even())
		},
		{
			# some ops can wrap others	
			# and_ takes a list of any size
			# ops.or_([ops]) can also be called
			msg='4 < x <= 9', 
			op=ops.and_([ops.gt(4), ops.lteq(9)])
		},
		{
			# use an expression operator
			# _x, _y are the default names for vars in ops.expr
			# this can be changed at the top of Operators.gd
			msg='x != 4 and x != 1', 
			op=ops.expr('_x != 4 and _x != 1')
		},
		{
			msg='x < 6', 
			op=ops.func_(self, 'less_than_6')
		},
	]
	var map_ops = [
		{
			# identity returns the item as is
			msg='x', 
			op=ops.identity()
		},
		{
			msg='x + 2', 
			op=ops.expr('_x + 2')
		},
		{
			# call built-in function from expr
			msg='x cubed',
			op=ops.expr('pow(_x, 3)')
		},
		{
			# named args can be passed to an expression
			msg='pow(x, y)',
			op=ops.expr('pow(_x, y)', {y=10})
		},
		{
			# if-else
			msg='if x < 5: pow(x, 2) else: x * 10',
			op=ops.if_(
				ops.lt(5), 
				ops.expr('pow(_x, 2)'), # then
				ops.expr('_x * 10')) # else
		},
		{
			# additional args passed to function
			msg='x + y',
			op=ops.func_(self, 'plus_xy', [2]),
		},
		{
			msg='x*y*z',
			op=ops.func_(self, 'mult_xyz', [5, 10]),
		},
		{
			# use a class that derives from OperatorBase (^above)
			msg='user-op: x / 2',
			op=Div.new(2),
		},
		{
			# comp(ose) passes each entry through a series of operators
			msg='(((x + 3) + y) > 8)',
			op=ops.comp([
				ops.expr('_x + 3'),
				ops.func_(self, 'plus_xy', [2])])
		},
		{
			msg='expr => {}\n',
			# if using '{}' in an expr use quotes outside and speech marks inside: 
			#   expr('{"key":value}')
			op=ops.expr('{"value": _x}')
		},
		{
			# dict_apply returns a Dictionary with the ops applied
			#  it will create new fields if they do not exist (in the data)
			msg='dict_apply => {}\n',
			# an Array will be treated as ops.comp(arr)
			op=[ops.expr('_x + 3'),
				ops.dict_apply({value=ops.identity(), is_even=ops.even()})]
		},
	]
	
	prints('data:', data)
	ex_util.pr_equals_break()
	
	printt('filter\n')
	
	var flex = fdb.flex()

	# filter returns all in data where the op returns true
	for f in filter_ops:
		printt(f.msg, flex.filter(f.op, data))
		ex_util.pr_dash_break()
	ex_util.pr_equals_break()

	printt('map\n')

	# map applies an op to each in data and returns the result
	for m in map_ops:
		printt(m.msg, flex.map(m.op, data))
		ex_util.pr_dash_break()
	ex_util.pr_equals_break()


	# ittr (iterator) passes the next item and previous result to the operator
	# in this case; adding them together
	# for this reason the function 'plus_xy' must take 2 args
	printt('add all items:',
		flex.ittr(ops.func_(self, 'plus_xy'), data))

	# reduce returns the last result from ittr
	printt('product:',
		fdb.reduce(ops.expr('_x * _y'), data.slice(1,-1)))

	# printt('product:',
	# 	fdb.flex()\
	# 		.filter('_x > 0')\
	# 		.reduce(ops.expr('_x * _y'), data))

	# not all operators will work with sort, ittr, reduce
	# expr, func_, lt/gt/eq for this purpose
	ex_util.pr_dash_break()
	print('Sorting\n')
	printt('sorted',
		fdb.sort(ops.lt(), data))
	printt('sorted (descending)',
			flex.sort('_x > _y', data))
	printt('sorted dict',
		fdb.sort(
			ops.expr('_x.x > _y.x'), 
			flex.map(ops.expr('{"x":_x}'), data)))


	ex_util.pr_dash_break()
	print('Arrays of primitives\n')
	var nested = [[0, [1, 2], 3], [4, [5, 6]], [7,[8],[9, 10]]]
	printt('data:', nested, '\n')
	
	var open_idx = [
		{
			msg='x[2] for each',
			op=ops.open_idx(2),
		},
		{
			msg='x[1][1] for each',
			op=ops.open_idx('1/1'),
		},
		{
			msg='[x[0], x[2]] for each',
			op=ops.open_idx([0, 2]),
		},
		{
			msg='[x[0], x[2], x[1][0], x[1][1]] for each',
			op=ops.open_idx(['0', 2, '1/0', [1,1]]),
		}
	]

	for o in open_idx:
		printt(o.msg, flex.map(o.op, nested))
