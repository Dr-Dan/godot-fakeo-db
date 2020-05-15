tool
extends EditorScript

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

const Enumerables = fdb.Enumerables

class E:
	extends Enumerables.List
	
	func _init(list).(list):
		pass

func mult2(itm):
	return itm * 20
"""
To use: File > Run
"""
	
func _run():
	var l = Enumerables.List.new([1,2,3,4,1,2])
	print(l.to_array())
	print(fdb.qb(l).take(2).to_array())
#	print(fdb.qb(l).first(ops.eq(2)))
#	print(fdb.qb(l).last(ops.eq(2)))
#	print(fdb.qb(l).exists(ops.eq(2)))

	var q = fdb.qb(l).where(ops.gteq(2)).skip(1)
	print(q.to_array())

	var l2 = Enumerables.List.new([{name="Dan", age=22, bag={food=5, matchbox={matches=20}}}, {name="Sally", age=42, bag={food=0}}])

	print(fdb.qb(l2).where(Enumerables.Operators.DictCompareOp.new({name=ops.eq("Dan")})).to_array())
	var q2 = fdb.qb(l2).where({name=ops.eq("Dan")})
	print(q2.to_array())

	var q3 = fdb.qb(l2).project_deep(["name", "bag/matchbox/matches", "bag/food"])
	print(q3.to_array())

	var e = E.new([1,3,4])
	var qe = fdb.qb(e).where(ops.gt(1))
	print(qe.to_array())
	
	var e2 = E.new([1,3,4])
	var qe2 = fdb.qb(e).select(ops.func_(self, "mult2"))
#	var qe2 = fdb.qb(e).select(ops.expr("_item * 10"))
	print(qe2.to_array())
	
	print(ops.expr("_item.bag.food").eval({name="Sally", age=42, bag={food=22}}))
	print(ops.expr("dmg*atk_rate", ["dmg", "atk_rate"]).eval({name="Holy Spear", age=42, dmg=2, atk_rate=1.75}))
	print(ops.expr_dp("matches*food", ["bag/matchbox/matches", "bag/food"]).eval(l2.at(0)))
	print(ops.open_dp("bag/matchbox").eval(l2.at(0)))

#	var qa = fdb.qb().select(self, "get_matches").where(ops.gteq(2))
#	var qa = fdb.qb().where_ref(funcref(self, "matches_gt", [2]))
#	var qa = fdb.qb().where({bag={matchbox={matches=ops.gteq(2)}}})
#	var qa = fdb.qb().add(["name"]).open("bag").add(["food", "mathbox/matches"])
#	{_=["name"], bag={matchbox="matches"}}
#	{
#		_find={name=ops.eq("Dan")}, 
#		_set={
#			bag={
#				matchbox={
#					matches=ops.set_val(5)
#				}}}}
# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")

# ==============================================================
