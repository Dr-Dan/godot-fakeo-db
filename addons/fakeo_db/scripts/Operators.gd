

static func item_valid(item, comps: Dictionary):
	for key in comps:
		if key in item:
			if not comps[key].eval(item[key]):
				return false
		else:
			return false
	return true

	
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

# ------------------------------------------------------------ 
# LOGICAL OPERATORS
"""
	returns true if all operators in 'cmps' are true
	Expects an array of operators

	usage:
		var comp = OpFac.And.new([OpFac.gt(20), OpFac.lt(70)]) # 20 < age < 70

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
		var comp = OpFac.Or.new([OpFac.gt(20), OpFac.lt(70)]) # 20 < age < 70

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
	returns false if 'cmp' is true and vice versa
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
# COMPARITIVE OPERATORS
# handle dictionary value only

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

# ------------------------------------------------------------ 
# FUNCREF

class FuncOpUtil:
	extends Resource
	
	static func get_func_op(func_ref:FuncRef, args:Array=[]):
		var func_op
		var n_args = args.size()
		assert(n_args <= FuncOpArgsBase.MAX_ARGS)	
		match n_args:
			0:
				return FuncOp.new(func_ref)
			1:
				return FuncOpArgs1.new(func_ref, args)
			2:
				return FuncOpArgs2.new(func_ref, args)
			3:
				return FuncOpArgs3.new(func_ref, args)
		return null
	
"""
Call a function on an object
	usage:
		func validate(item):
			...
			return true

		FuncOp.new(funcref(self, "validate"))	
"""
class FuncOp:
	extends OperatorBase
	
	var func_ref:FuncRef

	func _init(func_ref:FuncRef):
		self.func_ref = func_ref

	func eval(item):
		return func_ref.call_func(item)
			
class FuncOpArgsBase:
	extends OperatorBase
	const MAX_ARGS = 3
	
	var args:Array setget _set_args
	var n_args:int
	var func_ref:FuncRef
	
	func _init(func_ref:FuncRef, args:Array):
		n_args = args.size()
		assert(n_args <= MAX_ARGS)		
		self.func_ref=func_ref
		self.args = args
		
	func _set_args(args_):
		args = args_
		n_args = args.size()
		assert(n_args <= MAX_ARGS)	
		
	func eval(item):
		pass

class FuncOpArgs1:
	extends FuncOpArgsBase

	func _init(func_ref:FuncRef, args:Array=[]).(func_ref, args):		
		pass

	func eval(item):
		return func_ref.call_func(item, args[0])			

class FuncOpArgs2:
	extends FuncOpArgsBase

	func _init(func_ref:FuncRef, args:Array=[]).(func_ref, args):		
		pass

	func eval(item):
		return func_ref.call_func(item, args[0], args[1])

class FuncOpArgs3:
	extends FuncOpArgsBase

	func _init(func_ref:FuncRef, args:Array=[]).(func_ref, args):		
		pass

	func eval(item):
		return func_ref.call_func(item, args[0], args[1], args[2])	
