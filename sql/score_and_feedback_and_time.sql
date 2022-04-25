SET @preqsid = (select qsid from qsets where fid='OOPUY-PRE');
SET @postqsid = (select qsid from qsets where fid='OOPUY-POST');

CREATE OR REPLACE VIEW score AS
SELECT t.*, s.*
FROM
(
	SELECT
		k.fid AS topicid,
		k.type, k.qsid, k.qcount, k.qid, k.options, k.key, a.uid AS uid, a.qoid AS ans,
		IF(k.key = a.qoid, 1, 0) AS correct, m.mid, m.name AS topicname, a.answer_time
	FROM answers_mc_approved a
	LEFT JOIN
	(
		SELECT qs.fid, qs.qsid, qs.type, q.qid, q.answer_qoid AS `key`,
			GROUP_CONCAT(o.qoid) AS options,
			(
				SELECT COUNT(*) FROM qsets_has_questions qst WHERE qst.qsid = qs.qsid
			) AS qcount
		FROM qsets qs
		LEFT JOIN qsets_has_questions qq ON qq.qsid = qs.qsid
		LEFT JOIN questions q ON q.qid = qq.qid
		LEFT JOIN options o ON o.qid = q.qid
		GROUP BY qs.fid, qs.qsid, q.qid
	) k ON k.qsid = a.qsid AND k.qid = a.qid
	LEFT JOIN qsets qs ON qs.qsid = k.qsid
	LEFT JOIN materials m ON m.mid = qs.mid
) t LEFT JOIN (
	SELECT u.uid AS userid, u.username AS user, u.name, u.kid AS `group`
	FROM approvedUsers u # this is a view built in question.sql
) s ON s.userid = t.uid
-- WHERE s.user NOT LIKE '%hori%' -- test user
ORDER BY s.user, t.qsid;

select * from score;


select *
from score
where `group` = 41;

-- feedback count
select f.uid as userid, count(*) feedbacks
from logs_feedback f
where f.kid in (41)
  and uid in
      (
          select uid
          from approvedusers
      )
group by f.uid, f.kid;

## below queries are not fixed for oop_maya database
select distinct action from logs_cmapping where action like '%-kb%';

SET @j = '{"uid":178,"remaining":420}';
SELECT JSON_EXTRACT(@j,  '$.remaining');

-- time on task
SELECT l.uid as userid,
       (60*60) - JSON_EXTRACT(l.data,  '$.remaining') time_on_task
FROM logs_cmapping l
WHERE l.action = 'finish-kb' AND l.uid in (SELECT uid FROM approvedusers)
order by uid;

select * from approvedusers;

-- time on task + feedback count
SELECT l.lid, l.uid,
       (60*60) - JSON_EXTRACT(l.data,  '$.remaining') time_on_task,
        f.kid, f.feedbacks
FROM logs_cmapping l
LEFT JOIN (
select f.lid, f.uid, f.kid, count(*) feedbacks
                    from logs_feedback f
                    where f.kid in (41)
                      and f.uid in
                          (
                              select uid
                              from approvedusers
                          )
                    group by f.uid, f.kid
    ) f ON f.uid = l.uid
WHERE l.action = 'finish-kb' AND l.uid in (SELECT uid FROM approvedusers)
order by uid;
