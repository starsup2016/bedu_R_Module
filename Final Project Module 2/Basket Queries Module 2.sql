use basket;

Select * from mncaatourneydetailedresults where season>=2022;

-- Home Team "H"
-- Visiting Team "A"
-- Neutral COurt "N"
select * from mregularseasoncompactresults where WLoc not in ('H', 'N', 'A');
select count(*) from mregularseasoncompactresults where NumOT <> 0;

SELECT * FROM mncaatourneydetailedresults limit 100;

select b.TeamName, count(a.WTeamID) as MostWin from mregularseasoncompactresults as a join mteams as b on WTeamID = TeamID group by b.TeamName order by MostWin Desc limit 5;

-- We will use Duke and Kansas as subject to do the prediction of wins, also Chicago St to campare probability of win as the team historicaly with most losses
select * from mteams where TeamName='Duke';  -- Duke -> TeamID = 1181
select * from mteams where TeamName='Kansas'; -- Kansas -> TeamID = 1242
select * from mteams where TeamName='Chicago St'; -- Chicago St -> TeamID = 1152

-- Selection for Duke
select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1181 or LTeamID=1181;
select distinct(Season), count(TeamName) from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1181 or LTeamID=1181 group by Season, TeamName;
select distinct(Season), count(TeamName) from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1181 or LTeamID=1181 group by Season;
-- Selection for Kansas
select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1242 or LTeamID=1242;
-- Selection for Chicago St
select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1152 or LTeamID=1152;

-- 
select b.TeamName, count(a.LTeamID) as MostLosses from mregularseasoncompactresults as a join mteams as b on LTeamID = TeamID group by b.TeamName order by MostLosses Desc limit 5;