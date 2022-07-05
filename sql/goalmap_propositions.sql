select *
from goalmaps;


select @i := @i + 1 as '#', data.*
from (
         select gs.label source, l.label link, target.target, l.source source_id, l.lid link_id, target.cid target_id
         from (select @i := 0) r,
              goalmaps_links l
                  left join goalmaps_concepts gs on gs.gmid = l.gmid and gs.cid = l.source
                  left join (
                  select gt.label target, tl.cid, tl.lid
                  from goalmaps_links_target tl
                           left join goalmaps_concepts gt on gt.cid = tl.cid and gt.gmid = tl.gmid
                  where gt.gmid = 39
              ) target on target.lid = l.lid
         where l.gmid = 39
         order by source) data