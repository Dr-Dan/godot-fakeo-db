extends Resource
# the name that will be used to refer to the current
# 	object in an expression i.e. {dmg = 2} =>'_x.dmg * 4'
const EXPR_NAME = '_x'
const EXPR_NAME2 = '_y'

## =======================================================
## UTILS
class Util:
	static func get_map_op(input, args=[])\
		-> OperatorBase:
		if input is OperatorBase:
			return input
		elif input is String:
			if args is Dictionary:
				return ExprArgsDict.new(input, args)
			return ExprArgsDeep.new(input, args)
		elif input is FuncRef:
			return Func.new(input, args)
		elif input is Array:
			return OpenMultiDeep.new(input)
		elif input is Dictionary:
			return DictApplied.new(input, args)
		return null
	
	static func get_filter_op(input, args=[])\
		-> OperatorBase:
		if input is OperatorBase:
			return input
		elif input is FuncRef:
			return Func.new(input, args)
		elif input is String:
			return ExprArgsDeep.new(input, args)
		elif input is Dictionary:
			return DictCompare.new(input)
		return null
		

# ==============================================================
	
class OperatorBase:
	func eval(item):
		return false

class Value:
	extends OperatorBase
	var value
	func _init(value_):
		value = value_
		
	func eval(item):
		return value

class OperatorIterator:
	extends OperatorBase

	var ops = []
	var exit_op: OperatorBase = null
	var is_pred:bool
	
	func _init(ops_:Array, exit_op_:OperatorBase=null, is_predicate_:bool=false):
		ops = ops_
		exit_op = exit_op_
		is_pred = is_predicate_

	func eval(item):
		for op in ops:
			item = op.eval(item)
			if exit_op != null:
				var e = exit_op.eval(item)
				if not e:
					if is_pred:
						item = e
					break		
		return item

# ==============================================================
# DICTIONARY OPERATORS;

"""
Compare a dictionary of values with those in an object
Any naked values (i.e. 'field0' below) will be wrapped with an Eq operator

usage:
	DictCompare.new(
		{field0=value, field1=ops.eq(value), field2=ops.gt(value)}
	)

"""
class DictCompare:
	extends OperatorBase
	var fields
	var any:bool
	var fail_missing:bool

	func _init(fields: Dictionary, _any=false, _fail_missing=true):
		self.fields = fields
		for p in fields:
			if not fields[p] is OperatorBase:
				fields[p] = Eq.new(fields[p])
		any = _any
		fail_missing = _fail_missing

	func eval(item):
		return item_valid(item, fields)

	func item_valid(item, comps: Dictionary)->bool:
		for key in comps:
			if key in item:
				if comps[key].eval(item[key]):
					if any:
						return true
				else:
					return false
			elif fail_missing:
				return false
		return true		

			
# NOTE: will not actually stop user from mutating an object
class DictApplied:
	extends OperatorBase
	var fields:Dictionary
	var open:OpenMultiDeep

	func _all_ops(fields_:Dictionary):
		for v in fields_.values():
			if not v is OperatorBase and not v is String:
				return false
		return true

	func _init(_fields:Dictionary, _other:Array=[]).():
		fields = _fields
		for k in fields:
			fields[k] = Util.get_map_op(fields[k])
		assert(_all_ops(_fields))
		open = OpenMultiDeep.new(_other)

	func eval(item):
		return result(item, fields)

	func result(item, fields: Dictionary):
		var r = {}
		for key in fields:
			# if key in item:
			# 	r[key] = fields[key].eval(item[key])
			# else:
			r[key] = fields[key].eval(item)
		var other = open.eval(item)
		for k in other:
			r[k] = other[k]
		return r		

# ==============================================================
# EVALUATIVE OPERATORS; 'eval' should return a bool
		
# ------------------------------------------------------------ 
# LOGICAL
"""
	returns true if all operators in 'cmps' are true
	will exit early if a result is false
	Expects an array of operators

	usage:
		var comp = And.new([GT.new(20), LT.new(70)]) # 20 < age < 70

"""
class And:
	extends OperatorBase
	var cmps
	func _init(cmps:Array):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if not c.eval(item):
				return false
		return true

"""
	returns true if all operators in 'cmps' are true
	doesn't exit early if a result is false
	Expects an array of operators

	usage:
		var comp = All.new([GT.new(20), LT.new(70)]) # 20 < age < 70

"""
class All:
	extends OperatorBase
	var cmps
	func _init(cmps:Array):
		self.cmps = cmps
		
	func eval(item):
		var r = true
		for c in cmps:
			if not c.eval(item):
				r = false
		return r
	
"""
	returns true if any operators in 'cmps' are true
	Expects an array of operators

	usage:
		var comp = Or.new([Eq.new("Mike"), Eq.new("Anna")])

	"""
class Or:
	extends OperatorBase
	var cmps
	func _init(cmps:Array):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if c.eval(item):
				return true
		return false
				
"""
	returns false if 'cmp.eval(item)' returns true and vice versa
"""
class Not:
	extends OperatorBase
	var cmp
	
	func _init(cmp):
		self.cmp = cmp
		
	func eval(item):
		if cmp.eval(item):
			return false
		return true

"""
	returns false for any item
"""
class Is:
	extends OperatorBase
	var type
	func _init(type_):
		type = type_
		
	func eval(item):
		return item is type		
		
class IsVariantType:
	extends OperatorBase
	var type:int
	func _init(type_:int):
		type = type_
		
	func eval(item):
		return typeof(item) == type		
				
# ------------------------------------------------------------ 
# COMPARITIVE
"""
	Less than
"""
class Even:
	extends OperatorBase
	var val
		
	func eval(item):
		return item % 2 == 0
	
"""
	Less than
"""
class LT:
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item < val
	
"""
	Greater than
"""
class GT:
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item > val
			
"""
	Equal to
"""
class Eq:
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item == val
		
	func eval2(item0, item1):
		return item0 == item1	

# ------------------------------------------------------------ 
# COLLECTION OPERATORS
class In:
	extends OperatorBase
	var container
	func _init(container):
		self.container = container
		
	func eval(item):
		for i in container:
			if item == i: return true
		return false
		
	func eval2(item0, item1):
		return item0 in item1	
			
"""
	returns true if field in item. 
		i.e. 'item.field' exists
"""
class HasField:
	extends OperatorBase
	var field

	func _init(field: String):
		self.field = field

	func eval(item):
		if not field in item:
			return false
		return true


class Contains:
	extends OperatorBase
	var item
	func _init(item):
		self.item = item
		
	func eval(container):
		for i in container:
			if item == i: return true
		return false
#		return self.item in item

	func eval2(item0, item1):
		return item1 in item0	
			
# ==============================================================
# FUNCTION-Y; 'eval' can return anything
# ------------------------------------------------------------ 
# FUNCREF

"""
Call a function on an object
	usage:
		func validate(item):
			...
			return true

		Func.new(funcref(self, "validate"), [arg1, ..., argN])	
"""

class Func:
	extends OperatorBase
	
	var args:Array
	var func_ref:FuncRef

	func _init(func_ref:FuncRef, args:Array=[]):
		self.func_ref = func_ref
		self.args = args

	func eval(item):
		return func_ref.call_funcv([item] + args)

	func eval2(item0, item1):
		return func_ref.call_funcv([item0, item1] + args)
		
"""
 Use each element in 'item' as an argument to the target function.
 in eval, 'item' must be an array
"""
class FuncAsArgs:
	extends OperatorBase
	
	var func_ref:FuncRef

	func _init(func_ref:FuncRef):
		self.func_ref = func_ref

	func eval(item):
		return func_ref.call_funcv(item)


class Expr:
	extends OperatorBase
	
	var expr:Expression = Expression.new()
	var target
	var expr_str
	func _init(expr_str_:String, _target=null).():
		expr.parse(expr_str_, [EXPR_NAME])
		expr_str = expr_str_
		target = _target
		
	func eval(item):
		return expr.execute([item], target)

	func eval2(item0, item1):
		expr.parse(expr_str, [EXPR_NAME, EXPR_NAME2])
		return expr.execute([item0, item1], target)
				
class ExprArgs:
	extends OperatorBase
	
	var expr:Expression = Expression.new()
	var fields = []
	var target
	
	func _init(expr_str:String, fields:Array=[], _target=null).():
		for f in fields:
			self.fields.append(Open.new(f))
		expr.parse(expr_str, [EXPR_NAME] + fields)
		target = _target
		
	func eval(item):
		var args = []
		for f in fields:
			args.append(f.eval(item))
		return expr.execute([item] + args, target)

class ExprArgsDeep:
	extends OperatorBase
	
	var expr:Expression = Expression.new()
	var fields = []
	var target
	
	func _init(expr_str:String, fields:Array, _target=null).():
		var r = []
		for f in fields:
			var f_split = f.split("/")
			r.append(f_split[f_split.size()-1])
			self.fields.append(OpenDeep.new(f))
		expr.parse(expr_str,  [EXPR_NAME] + r)
		target = _target
		
	func eval(item):
		var args = []
		for f in fields:
			args.append(f.eval(item))
		return expr.execute([item] + args, target)
	

class ExprArgsDict:
	extends OperatorBase
	
	var expr:Expression = Expression.new()
	var fields = {}
	var target
	var expr_str:String
	func _init(expr_str_:String, _fields:Dictionary={}, _target=null).():
		fields = _fields
		expr.parse(expr_str_, [EXPR_NAME] + fields.keys())
		expr_str = expr_str_
		target = _target
		
	func eval(item=null):
		return expr.execute([item] + fields.values(), target)
	
	func eval2(item0, item1):
		expr.parse(expr_str, [EXPR_NAME, EXPR_NAME2] + fields.keys())
		return expr.execute([item0, item1] + fields.values(), target)
		
class Open:
	extends OperatorBase
	
	var field:String
	
	func _init(field:String).():
		self.field = field

	func eval(item):
		var result = null
		if field in item:
			result = item[field]
		return result
		
class OpenMulti:
	extends OperatorBase

	var fields:Array
	
	func _init(_fields:Array).():
		fields = _fields

	func eval(item):
		var n = {}
		for f in fields:
			if f in item:
				n[f] = item[f]
		return n
	
class OpenDeep:
	extends OperatorBase
	
	var fields:Array = []
	
	func _init(field:String).():
		var f_split = field.split("/")
		for s in f_split:
			fields.append(s)

	func eval(item):
		var result = null
		for f in fields:
			if f == "*":
				result = item # TODO: test; should be item[f]?
				continue
			# assert(f in item)
			if f in item:
				result = item[f]
				item = result
				
			else: return null
		return result

		
# TODO: this should be up with dict functions? Or be renamed. (Project?)
# ValuesMulti is more inline with other OpenXXX classes
class OpenMultiDeep:
	extends OperatorBase

	# filled with OpenDeep
	var ops:Dictionary = {}
	
	func _init(_fields:Array).():
		for f in _fields:
			ops[f] = OpenDeep.new(f)
			assert(not ops[f].fields.empty())

	func eval(item):
		var result = {}
#		for f in ops:
#			var name = f.fields.back()
#			result[name] = f.eval(item)
		for f in ops:
			if not ops[f].fields.empty():
				var name = ops[f].fields.back()
				result[name] = ops[f].eval(item)
		return result

class ApplyToResult:
	extends OperatorBase
	
	var op0
	var op1
	
	func _init(op0_, op1_):
		op0 = op0_
		op1 = op1_
		
	func eval(x):
		var r = op0.eval(x)
		if r != null:
			return op1.eval(r)
		return false
		
# returns in the order provided an array of the requested fields
# ['name', 'age'] => ['mike', 43]
#class ValueMulti:
#	extends OperatorBase
#
#	# filled with OpenDeep
#	var ops:Array = []
#
#	func _init(_fields:Array).():
#		for f in _fields:
#			ops.append(OpenDeep.new(f))
#			assert(not ops.back().fields.empty())
#
#	func eval(item):
#		var result = []
#		for f in ops:
#			if not f.fields.empty():
#				result.append(f.eval(item))
#		return result
