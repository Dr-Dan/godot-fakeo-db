const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpUtil = Operators.Util
const Exit = [null, false]
	
class Processor:
	extends Resource
	
	func next(item, data={}):
		return [item, true, false]
		
	func make_data():
		return {}

class Map:
	extends Processor

	func _init().():
		pass

	func get_result(item, data):
		return item

	func next(item, data={}):	
		return [get_result(item, data), true, false]
	
class MapOp:
	extends Map
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	var pred_op
	
	func _init(pred_op:OpBase):
		self.pred_op = pred_op
		
	func get_result(item, data):
		return pred_op.eval(item)

class Filter:
	extends Processor

	func _init().():
		pass

	func get_result(item, data):
		return true

	func next(item, data={}):
		return [item, get_result(item, data), false]

class FilterIndexed:
	extends Filter

	func make_data():
		return {i=0}
		
	func next(item, data={}):
		var r = .next(item, data)
		data.i += 1
		return r
	
class FilterOp:
	extends Filter
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	var pred_op
	
	func _init(pred_op:OpBase).():
		self.pred_op = pred_op
		
	func get_result(item, data):
		return pred_op.eval(item)

class MapOpAuto:
	extends MapOp
	func _init(input, args={}).(OpUtil.get_map_op(input, args)):
		pass
	
class FilterOpAuto:
	extends FilterOp
	func _init(input, args={}).(OpUtil.get_filter_op(input, args)):
		pass

class TakeWhile:
	extends Filter
	var pred_op
	
	func _init(pred_op_):
		pred_op = pred_op_
		
	func make_data():
		return {t=false}

	func get_result(item, data):
		var r = pred_op.eval(item)
		data.t = not r
		return r
		
	func next(item, data={}):
		var r = .next(item)
		r[2] = data.t
		return r 
		
class Take:
	extends FilterIndexed
	var count: int
	
	func _init(count:int):
		self.count = count

	func next(item, data={}):
		var r = .next(item, data)
		r[2] = data.i >= count
		return r 

class Skip:
	extends FilterIndexed
	var count: int
	
	func _init(count:int):
		self.count = count
		
	func make_data():
		return {i=0}
		
	func get_result(item, data={}):
		return data.i >= count
