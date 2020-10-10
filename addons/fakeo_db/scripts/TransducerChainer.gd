extends "Transducers.gd".ProcIterator

const Itbl = preload("res://addons/fakeo_db/scripts/Iterable.gd")
const prc = preload("res://addons/fakeo_db/scripts/TransducerFactory.gd")
const ops = preload("res://addons/fakeo_db/scripts/Operators.gd")

func _init(procs_:Array=[]).(procs_) -> void:
	pass

func itbl(coll=[]):
	return Itbl.new(self, coll)
	
func apply(coll):
	return itbl(coll).run()
	
# these use different init from base so must be overwritten
func append(item):
	return get_script().new(procs + [item])
	
func append_array(items:Array):
	return get_script().new(procs + items)
	

func comp(input):
	return append(prc.comp(input))

func map(input):
	return append(prc.map(ops.Util.get_map_op(input)))

func filter(input):
	return append(prc.filter(ops.Util.get_filter_op(input)))

func project(input):
	return append(prc.project(input))

func take(amt: int):
	return append(prc.take(amt))

func take_while(op):
	return append(prc.take_while(op))

func skip(amt: int):
	return append(prc.skip(amt))

func ittr(op):
	return append(prc.ittr(op))
