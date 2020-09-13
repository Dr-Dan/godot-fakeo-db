const OpUtil = preload("res://addons/fakeo_db/scripts/Operators.gd").Util
const Exit = [null, false]
	
class Processor:
	extends Resource
	
	func next(item, data):
		return [item, true, false]

	func make_data():
		return {}

	# allows for faster impls over lists
	# func apply(coll):
	# 	var data = make_data()
	# 	var result = []
	# 	for n in coll:
	# 		var r = next(n, data)
	# 		if r[1]:
	# 			result.append(r[0])
	# 		if r[2]: break
	# 	return result	

class ProcIterator:
	extends Processor

	var procs:Array = []
	func _init(procs_:Array):
		procs = procs_
	
	func make_data():
		var proc_data = []
		for p in procs:
			proc_data.append(p.make_data())		
		return {proc_data=proc_data}
			
	func next(item, data):	
		var r = [item, true, false]
		var t = false
		for i in procs.size():
			var q = procs[i]
			r = q.next(r[0], data.proc_data[i])
			if r[2]: t = true
			if not r[1]: break # or r[2]?
		if t: r[2] = true
		return r


class MapOp:
	extends Processor
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	var pred_op
	
	func _init(pred_op:OpBase):
		self.pred_op = pred_op
		
	func get_result(item, data):
		return pred_op.eval(item)

	func next(item, data):	
		return [get_result(item, data), true, false]
	

class IterateOp:
	extends Processor
	const Ops = preload("res://addons/fakeo_db/scripts/Operators.gd")
	const OpBase = Ops.OperatorBase
	const OpFunc = Ops.Func
	
	var mut_op
	var def_val
	func make_data():
		return {started=false, value=def_val}

	func _init(mut_op_:OpBase, def_val_=null):
		mut_op = mut_op_
		def_val = def_val_

	func get_result(item, data):
		if not data.started:
			data.started = true
			data.value = item
		else:
			data.value = mut_op.eval2(item, data.value)
		return data.value

	func next(item, data):	
		return [get_result(item, data), true, false]	
		
class Filter:
	extends Processor

	func _init().():
		pass

	func get_result(item, data):
		return true

	func next(item, data):
		return [item, get_result(item, data), false]

class FilterIndexed:
	extends Filter

	func make_data():
		return {i=0}
		
	func next(item, data):
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
		
	func next(item, data):
		var r = .next(item, data)
		r[2] = data.t
		return r 

class Slice:
	extends FilterIndexed
	var st_idx: int
	var end_idx: int
	
	func _init(st_idx_:int, end_idx_:int):
		st_idx = st_idx_
		end_idx = end_idx_

	func next(item, data):
		var r = .next(item, data)
		r[1] = data.i >= st_idx
		r[2] = data.i >= end_idx
		return r 
		
class Take:
	extends FilterIndexed
	var count: int
	
	func _init(count:int):
		self.count = count

	func next(item, data):
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