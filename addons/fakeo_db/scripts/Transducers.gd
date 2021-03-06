const OpUtil = preload("res://addons/fakeo_db/scripts/Operators.gd").Util

class None:
	func _init():
		pass

class Terminate:
	func _init():
		pass


class Transducer:
	extends Resource
	
	func next(item, data):
		return item

	func make_data():
		return {}

	# TODO: for optional faster impls over lists?
	# func apply(coll):
	# 	var data = make_data()
	# 	var result = []
	# 	for n in coll:
	# 		var r = next(n, data)
	# 		if r[1]:
	# 			result.append(r[0])
	# 		if r[2]: break
	# 	return result	

class TdxIterator:
	extends Transducer

	var procs:Array = []
	func _init(procs_:Array=[]):
		procs = procs_
	
	func make_data():
		var proc_data = []
		for p in procs:
			proc_data.append(p.make_data())		
		return {proc_data=proc_data}
			
	func next(item, data):	
		var r = item
		var t = false
		for i in procs.size():
			var q = procs[i]
			r = q.next(r, data.proc_data[i])
			if r is Terminate or r is None:
				 break
		return r


class MapOp:
	extends Transducer
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	var pred_op
	
	func _init(pred_op:OpBase):
		self.pred_op = pred_op
		
	func get_result(item, data):
		return pred_op.eval(item)

	func next(item, data):	
		return get_result(item, data)
	
class Enumerate:
	extends Transducer

	var key:String
	var step:int
	var wrap:bool

	func _init(key_='i', step_:int=1, wrap_:bool=false):
		key = key_
		step = max(1, step_)
		wrap = wrap_
		
	func make_data():
		return {id=0}

	func next(item, data):	
		if wrap:
			item = {key:data.id, item=item}
		else:
			item[key] = data.id
		data.id += step
		return item
	

class IterateOp:
	extends Transducer
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
			data.value = mut_op.eval2(data.value, item)
		return data.value

	func next(item, data):	
		return get_result(item, data)
		
class Filter:
	extends Transducer

	func _init().():
		pass

	func get_result(item, data):
		return true

	func next(item, data):
		var r = get_result(item, data)
		if r:
			return item
		return None.new()

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

class TakeWhile:
	extends Filter
	var pred_op
	
	func _init(pred_op_):
		pred_op = pred_op_
		
	func make_data():
		return {terminal=false}

	func get_result(item, data):
		var r = pred_op.eval(item)
		data.terminal = not r
		return r
		
	func next(item, data):
		var r = .next(item, data)
		if data.terminal:
			return Terminate.new()
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
		if data.i >= st_idx:
			return None.new() 
		if data.i >= end_idx:
			return Terminate.new()
		return r 
		
class Take:
	extends FilterIndexed
	var count: int
	
	func _init(count:int):
		self.count = count

	func next(item, data):
		var r = .next(item, data)
		if data.i > count:
			return Terminate.new()
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
