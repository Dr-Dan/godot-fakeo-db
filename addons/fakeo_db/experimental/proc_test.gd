tool
extends EditorScript

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")
const QB = preload("res://addons/fakeo_db/scripts/Query.gd")
const QBR = preload("res://addons/fakeo_db/scripts/QueryIterable.gd")
const QR = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

var name_table = [
	{name="mike", age=22, addr_id=0, bag={hat=1}},
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=49, addr_id=2, bag={hat=2}},
	{name="aslan", age=199, addr_id=1},
	]
	
func _run() -> void:
	var sk = Proc.Skip.new(2)
	var tk = Proc.Take.new(3)
	var q0 = [sk, tk]
#	print(run_qry_coll(q0, get_qry_data(q0), name_table))
#	print(fdb.qry_proc(q0).apply(name_table))
	
	var p = Proc.WhereOp.new(ops.dict({age=ops.gteq(20)}))	
	var p2 = Proc.WhereDict.new({age=ops.lteq(100)})
	
	var po = Proc.WhereExpr.new('age >= 20 and age <= 100', ["age"])
	
	var qr = QR.new([po], name_table)
#	print(qr.run())
#	for i in qr: print(i)
#	print(fdb.qry_proc([Proc.TakeWhile.new(ops.expr('_item.age < 40'))]).apply(name_table))
	print(fdb.qry()\
		.where("hat", ["bag/hat"])\
		.project(["bag/hat", "name"])\
		.apply(name_table))
	# TODO: show this in examples
#	var ps = Proc.SelectOp.new(ops.dict({age=ops.gt(30)}))
#	var ps = Proc.SelectOp.new(ops.open(["age", "name"]))
	var ps = Proc.SelectOp.new(ops.open_dp(["bag/hat", "name"]))
#	print(fdb.qry_proc([ps, Proc.Take.new(2)]).run(name_table))
	
	var qb = QB.new().where({age=ops.lteq(100)}).select_op(ops.open(["age", "name"]))
	var qbr = QBR.new(name_table, []).where({age=ops.lteq(100)}).select_op(ops.open(["age", "name"]))
#	for i in qbr: print(i)
#	print(qbr.apply(name_table))
#	print(qb.apply(name_table))
	


	# TODO map as opposed to reduce. i.e. project with mutations
#	var ps2 = Proc.ProjectOp.new(ops.dict({age=ops.expr('_item * 10')}))
#	var ps2 = Proc.SelectOp.new(ops.expr('{"age":age * 4, "name":name}', ["age", "name"]))
#	var ps2 = Proc.SelectOp.new(ops.expr('{"name":_item.name, "age":_item.age * 10}'))

	var qry = [p]
	var runner = fdb.qry(qry).proc(Proc.Take.new(2)).skip(1).to_iter(name_table)
#	for i in runner:
#		print(i)

