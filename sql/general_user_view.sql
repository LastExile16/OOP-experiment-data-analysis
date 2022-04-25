select distinct u.uid, u.username, u.name
from nawras_maya_oop.users u,
     nawras_maya_oop.logs_cmapping l
where action like '%finish-post%'
  and day(l.time) = 28
  and u.uid > 100
  and u.uid = l.uid;


select distinct uid
from nawras_maya_oop.logs_cmapping
where action = 'sign-in'
  and uid > 400
  and day(time) = 28
order by uid

select *
from users
where uid in (
    select distinct uid from nawras_maya_oop.users_sessions where uid > 400 order by uid)

select distinct s.uid
from nawras_maya_oop.users_sessions s
where s.uid > 400
and json_contains(JSON_EXTRACT(s.data,  '$.activity'), '"kb"', '$')
order by s.uid;

select distinct s.uid
from nawras_maya_oop.users_sessions s
where s.uid > 400
and JSON_EXTRACT(s.data,  '$.user') is not null
order by s.uid;

select s.uid
from nawras_maya_oop.users_sessions s
where s.uid > 400
and JSON_EXTRACT(s.data,  '$.user') is not null
group by uid
having count(s.data)>1
order by s.uid;