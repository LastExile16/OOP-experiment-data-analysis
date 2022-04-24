-- The number of students who answered correctly and incorrectly in pre, post, and delay
SET @preqsid = (select qsid from qsets where fid='OOPUY-PRE');
SET @postqsid = (select qsid from qsets where fid='OOPUY-POST');
#SET @delayqsid = (select qsid from qsets where fid='OOPUY-DELAY');

# exclude username groups interchangeably to create question summary for both groups independently

-- create a view for approved users (those who finished upto finish-post-test)
CREATE OR REPLACE VIEW approvedUsers AS
SELECT *
FROM users
WHERE uid in
      (select u.uid
       from users u,
            logs_cmapping l
       where
         l.action = 'finish-post-test'
         and l.uid = u.uid);


CREATE OR REPLACE VIEW answers_mc_approved AS
SELECT a.*
FROM answers_mc a
         INNER JOIN approvedUsers u on u.uid = a.uid;

-- answer of all who did pre and post
CREATE OR REPLACE VIEW answers_mc_prePost AS
SELECT m.*
FROM answers_mc m
         INNER JOIN (
            SELECT DISTINCT a.uid u FROM answers_mc a
	WHERE a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = 1
	) AND a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = 2
	)
        ) usr on usr.u = m.uid;

-- All answer summary
SELECT q.qid, q.answer_qoid AS `key`,

	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND a.qoid = q.answer_qoid
	) AS pre_correct,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND a.qoid = q.answer_qoid
	) AS post_correct
# 	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND a.qoid = q.answer_qoid
# 	) AS delay_correct
	, (
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL)
	) AS pre_incorrect,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL)
	) AS post_incorrect
#	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL)
# 	) AS delay_incorrect
FROM questions q WHERE qid IN
	(SELECT DISTINCT qq.qid FROM qsets_has_questions qq WHERE qq.qsid = @preqsid)
ORDER BY qid;

-- Experimental group answer summary
SELECT q.qid, q.answer_qoid AS `key`,

	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND a.qoid = q.answer_qoid AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid=41)
	) AS pre_correct,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND a.qoid = q.answer_qoid AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid=41)
	) AS post_correct
# 	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND a.qoid = q.answer_qoid
# 	) AS delay_correct
	, (
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL) AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid=41)
	) AS pre_incorrect,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL) AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid=41)
	) AS post_incorrect
#	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL)
# 	) AS delay_incorrect
FROM questions q WHERE qid IN
	(SELECT DISTINCT qq.qid FROM qsets_has_questions qq WHERE qq.qsid = @preqsid)
ORDER BY qid;

-- Control group answer summary
SELECT q.qid, q.answer_qoid AS `key`,

	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND a.qoid = q.answer_qoid AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid IS NULL)
	) AS pre_correct,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND a.qoid = q.answer_qoid AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid IS NULL)
	) AS post_correct
# 	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND a.qoid = q.answer_qoid
# 	) AS delay_correct
	, (
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @preqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL) AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid IS NULL)
	) AS pre_incorrect,
	(
		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @postqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL) AND a.uid in ( SELECT au.uid FROM approvedUsers au WHERE au.kid IS NULL)
	) AS post_incorrect
#	, (
# 		SELECT COUNT(*) FROM answers_mc_approved a WHERE a.qid = q.qid AND a.qsid = @delayqsid AND (a.qoid <> q.answer_qoid OR a.qoid IS NULL)
# 	) AS delay_incorrect
FROM questions q WHERE qid IN
	(SELECT DISTINCT qq.qid FROM qsets_has_questions qq WHERE qq.qsid = @preqsid)
ORDER BY qid;

-- Who answered pre-test
SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @preqsid;
-- Who answered post-test
SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @postqsid;
-- Who answered pre-test but didn't answer the post-test
SELECT DISTINCT uid FROM answers_mc a
	WHERE a.qsid = @preqsid AND a.uid NOT IN
	(SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @postqsid);
-- Who answered post-test but didn't answer the pre-test
SELECT DISTINCT uid FROM answers_mc a
	WHERE a.qsid = @postqsid AND a.uid NOT IN
	(SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @preqsid);
-- Who answered delay-test
# SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @delayqsid;

-- Who answered pre-post-delay
SELECT DISTINCT a.uid FROM answers_mc a
	WHERE a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @preqsid
	) AND a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @postqsid
	);
# 	AND a.uid IN (
# 		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @delayqsid
# 	);

-- Which is incomplete
SELECT DISTINCT x.uid FROM answers_mc x
WHERE x.uid NOT IN (
	SELECT DISTINCT a.uid FROM answers_mc a
	WHERE a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @preqsid
	) AND a.uid IN (
		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @postqsid
    )
# 	) AND a.uid IN (
# 		SELECT DISTINCT uid FROM answers_mc a WHERE a.qsid = @delayqsid
# 	)
);