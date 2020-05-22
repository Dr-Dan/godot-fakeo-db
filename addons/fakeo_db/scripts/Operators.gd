
# ==============================================================
	
class OperatorBase:
	func eval(item):
		return false
	
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


# ==============================================================
# EVALUATIVE OPERATORS; 'eval' should return a bool

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

	func item_valid(item, comps: Dictionary):
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

# ------------------------------------------------------------ 
# LOGICAL
"""
	returns true if all operators in 'cmps' are true
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
	returns true for any item
"""
class Any:
	extends OperatorBase

	func _init():
		pass
		
	func eval(item):
		return true
	
"""
	returns false for any item
"""
class None:
	extends OperatorBase

	func _init():
		pass
		
	func eval(item):
		return false
# ------------------------------------------------------------ 
# COMPARITIVE

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
					
# ------------------------------------------------------------ 
# LIST OPERATORS
class In:
	extends OperatorBase
	var container
	func _init(container):
		self.container = container
		
	func eval(item):
		for i in container:
			if item == i: return true
		return false
		
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
	
	func _init(expr_str:String, _target=null).():
		expr.parse(expr_str, ["_item"])
		target = _target
		
	func eval(item):
		return expr.execute([item], target)
		
class ExprArgs:
	extends OperatorBase
	
	var expr:Expression = Expression.new()
	var fields = []
	var target
	
	func _init(expr_str:String, fields:Array=[], _target=null).():
		for f in fields:
			self.fields.append(Open.new(f))
		expr.parse(expr_str, ["_item"] + fields)
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
		expr.parse(expr_str,  ["_item"] + r)
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
	
	func _init(expr_str:String, _fields:Dictionary={}, _target=null).():
		fields = _fields
		expr.parse(expr_str, ["_item"] + fields.keys())
		target = _target
		
	func eval(item=null):
		return expr.execute([item] + fields.values(), target)
		
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
		
class Open:
	extends OperatorBase
	
	var field:String
	
	func _init(field:String).():
		self.field = field

	func eval(item):
		var result = null
		assert(field in item)
		if field in item:
			result = item[field]
			# item = result
		return result
						

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
				result = item
				continue
			# assert(f in item)
			if f in item:
				result = item[f]
				item = result
				
			else: return null
		return result
	
class OpenMultiDeep:
	extends OperatorBase

	var fields:Dictionary = {}
	
	func _init(_fields:Array).():
		for f in _fields:
			fields[f] = OpenDeep.new(f)

	func eval(item):
		var result = {}
		for f in fields:
			if not fields[f].fields.empty():
				var name = fields[f].fields.back()
				# if f in item:
				result[name] = fields[f].eval(item)
		return result
