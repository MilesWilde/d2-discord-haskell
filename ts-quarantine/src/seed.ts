require("dotenv").config();

interface CharStat {
    class: string;
    str: number;
    dex: number;
    int: number;
    vit: number;
    tot: number;
    hpadd: number;
    LifePerLevel: number;
    ManaPerLevel: number;
    LifePerVitality: number;
    ManaPerMagic: number;
}

const knex = require("knex")({
    client: "pg",
    version: 13.3,
    connection: {
        host: process.env.PG_HOST,
        user: process.env.PG_USER,
        password: process.env.PG_PASSWORD,
        database: process.env.PG_DB_NAME,
    },
});

const pg = require("knex")({
    client: "pg",
    connection: process.env.PG_CONNECTION_STRING,
    searchPath: ["knex", "public"],
});

var CharStats = require("../../d2data/charstats.json") as Record<
    string,
    CharStat
>;
const seedCharStats = () => {
    try {
        Object.entries(CharStats).forEach(async ([name, curStats]) => {
            const trx = await knex.transaction();
            if (name.toLowerCase() != "expansion") {
                await trx("gdclasses")
                    .insert({
                        class: curStats.class,
                        dex: curStats.dex,
                        str: curStats.str,
                        vit: curStats.vit,
                        nrg: curStats.int,
                        hpadd: curStats.hpadd,
                        life_pl: curStats.LifePerLevel,
                        mana_pl: curStats.ManaPerLevel,
                        life_p_vit: curStats.LifePerVitality,
                        mana_p_nrg: curStats.ManaPerMagic,
                    })
                    .then((res: number) => {
                        console.log(res);
                    })
                    .then(trx.commit);
            }
        });
    } catch (err) {
        console.log(err);
    }
};

interface MonsterLevel {
    Level: string;
    monsterLevelDifficultyVariables: {
        normal: MonsterLevelDifficultyVariables;
        hell: MonsterLevelDifficultyVariables;
        nightmare: MonsterLevelDifficultyVariables;
    };
}

interface MonsterLevelDifficultyVariables {
    AC: number;
    TH: number;
    HP: number;
    DM: number;
    XP: number;
}
var MonsterLevels = require("../../d2data/monlvlNew.json") as MonsterLevel[];

const seedMonLvl = () => {
    try {
        MonsterLevels.forEach((monLvl) => {
            Object.entries(monLvl.monsterLevelDifficultyVariables).forEach(
                async ([, monLvlDiffVars], i) => {
                    const trx = await knex.transaction();
                    await trx("monlvls")
                        .insert({
                            ac: monLvlDiffVars.AC,
                            th: monLvlDiffVars.TH,
                            hp: monLvlDiffVars.HP,
                            dam: monLvlDiffVars.DM,
                            xp: monLvlDiffVars.XP,
                            level: monLvl.Level,
                            difficulty: i,
                        })
                        .then((res: number) => {
                            console.log(res);
                        })
                        .then(trx.commit);
                }
            );
        });
    } catch (err) {
        console.log(err);
    }
};

interface Monster {
    Id: string;
    BaseId: string;
    NextInClass: string;
    NameStr: string;
    MonStatsEx: string;
    MonType: string;
    AI: string;
    Code: string;
    MonSound: string;
    UMonSound: string;
    Skill1: string;
    Sk1mode: string;
    hcIdx: number;
    enabled: number;
    MinGrp: number;
    MaxGrp: number;
    PartyMin: number;
    PartyMax: number;
    Velocity: number;
    Run: number;
    Rarity: number;
    threat: number;
    isSpawn: number;
    isMelee: number;
    lUndead: boolean;
    hUndead: number;
    opendoors: number;
    killable: number;
    switchai: number;
    zoo: number;
    Sk1lvl: number;
    DamageRegen: number;
    Crit: number;
    minHP: number;
    maxHP: number;
    demon: number;
    npc: number;
    minions: string[];
    monsterDifficultyVariables: {
        normal: MonsterDifficultyVariables;
        hell: MonsterDifficultyVariables;
        nightmare: MonsterDifficultyVariables;
    };
}

interface MonsterDifficultyVariables {
    treasureClasses: string[];
    Level: number;
    aidel: number;
    aip1: number;
    aip2: number;
    aip3: number;
    aip4: number;
    coldeffect: number;
    ResDm: number;
    ResLi: number;
    ResPo: number;
    ResMa: number;
    ResCo: number;
    ResFi: number;
    ToBlock: number;
    MinHP: number;
    MaxHP: number;
    AC: number;
    Exp: number;
    A1MinD: number;
    A1MaxD: number;
    A1TH: number;
    A2MinD: number;
    A2MaxD: number;
    A2TH: number;
}

var MonsterStats = require("../../d2data/monstatsNew.json") as Record<
    string,
    Monster
>;

const seedMonsterStats = () => {
    try {
        Object.entries(MonsterStats).forEach(async ([monId, monStats]) => {
            const statTrx = await knex.transaction();
            let isUndead = false;
            if (monStats.lUndead || monStats.hUndead) {
                isUndead = true;
            }
            let isDemon = false;
            if (monStats.demon) {
                isDemon = true;
            }
            await statTrx("monstats")
                .insert({
                    minions: monStats.minions
                        ? convertStringListToString(monStats.minions)
                        : convertStringListToString([]),
                    name: monStats.NameStr,
                    code: monStats.Code,
                    type: monStats.MonType ? monStats.MonType : "",
                    party_min: monStats.PartyMin ? monStats.PartyMin : 0,
                    party_max: monStats.PartyMax ? monStats.PartyMax : 0,
                    grp_min: monStats.MinGrp,
                    grp_max: monStats.MaxGrp,
                    rarity: monStats.Rarity ? monStats.Rarity : 0,
                    text_id: monId,
                    undead: isUndead,
                    demon: isDemon,
                })
                .then((res: number) => {
                    console.log(res);
                })
                .then(statTrx.commit)
                .finally(console.log("done1"));

            Object.entries(monStats.monsterDifficultyVariables).forEach(
                async ([, monStatsDiffVars], i) => {
                    const diffStatTrx = await knex.transaction();
                    await diffStatTrx("mondiffstats")
                        .insert({
                            level: monStatsDiffVars.Level
                                ? monStatsDiffVars.Level
                                : 0,
                            res_dm: monStatsDiffVars.ResDm
                                ? monStatsDiffVars.ResDm
                                : 0,
                            res_ma: monStatsDiffVars.ResMa
                                ? monStatsDiffVars.ResMa
                                : 0,
                            res_fi: monStatsDiffVars.ResFi
                                ? monStatsDiffVars.ResFi
                                : 0,
                            res_li: monStatsDiffVars.ResLi
                                ? monStatsDiffVars.ResLi
                                : 0,
                            res_co: monStatsDiffVars.ResCo
                                ? monStatsDiffVars.ResCo
                                : 0,
                            res_po: monStatsDiffVars.ResPo
                                ? monStatsDiffVars.ResPo
                                : 0,
                            min_hp: monStatsDiffVars.MinHP
                                ? monStatsDiffVars.MinHP
                                : 0,
                            max_hp: monStatsDiffVars.MaxHP
                                ? monStatsDiffVars.MaxHP
                                : 0,
                            ac: monStatsDiffVars.AC ? monStatsDiffVars.AC : 0,
                            exp: monStatsDiffVars.Exp
                                ? monStatsDiffVars.Exp
                                : 0,
                            treasure_classes: monStatsDiffVars.treasureClasses
                                ? convertStringListToString(
                                      monStatsDiffVars.treasureClasses
                                  )
                                : convertStringListToString([]),
                            difficulty: i,
                            text_id: monId,
                        })
                        .then((res: number) => {
                            console.log(res);
                        })
                        .then(diffStatTrx.commit)
                        .finally(console.log("done2"));
                }
            );
        });
    } catch (err) {
        console.log(err);
    }
};

const adjacentZones = require("../../d2data/adjacentZoneMap.json") as Record<
    string,
    string[]
>;

const seedAdjacentZones = () => {
    try {
        Object.entries(adjacentZones).forEach(async ([zone, adjacentZones]) => {
            const trx = await knex.transaction();
            await trx("adjzones")
                .insert({
                    zone: zone,
                    adjacent_zones: adjacentZones,
                })
                .then((res: number) => {
                    console.log(res);
                })
                .then(trx.commit)
                .finally(console.log("done"));
        });
    } catch (err) {
        console.log(err);
    }
};

interface Level {
    NormalMonsters: string[];
    NightmareAndHellMonsters: string[];
    UniqueMonsters: string[];
    Critters: string[];
    CritterSpawnPercentages: number[];
    VisList: number[];
    Warps: number[];
    ObjectGroups: number[];
    ObjectProbabilities: number[];
    MonsterLevels: number[];
    MonsterLevelsExpansion: number[];
    levelDifficultyVariables: {
        normal: LevelDifficultyVariables;
        hell: LevelDifficultyVariables;
        nightmare: LevelDifficultyVariables;
    };
    Name: string;
    LevelName: string;
    LevelWarp: string;
    EntryFile: string;
    Id: number;
    Pal: number;
    Act: number;
    Quest: number;
    QuestPart: number;
    QuestFlag: number;
    QuestFlagEx: number;
    Layer: number;
    OffsetX: number;
    OffsetY: number;
    Teleport: number;
    LOSDraw: number;
    FloorFilter: number;
    BlankScreen: number;
    IsInside: number;
    DrlgType: number;
    LevelType: number;
    SubType: number;
    SubTheme: number;
    SubWaypoint: number;
    SubShrine: number;
    Red: number;
    Green: number;
    Blue: number;
    SaveMonsters: number;
    WarpDist: number;
    MonWndr: number;
    NumMon: number;
    Themes: number;
    SoundEnv: number;
    Waypoint: number;
}

interface LevelDifficultyVariables {
    SizeX: number;
    SizeY: number;
    MonDen: number;
    MonUMin: number;
    MonUMax: number;
}

var zoneData = require("../../d2data/LevelsNew.json") as Record<string, Level>;

const seedZoneData = () => {
    try {
        Object.entries(zoneData).forEach(async ([zoneName, zone]) => {
            const zoneTrx = await knex.transaction();
            let town = zone.NumMon ? false : true;
            await zoneTrx("zonelvl")
                .insert({
                    norm_mons: convertStringListToString(zone.NormalMonsters),
                    nmh_mons: convertStringListToString(
                        zone.NightmareAndHellMonsters
                    ),
                    unique_mons: convertStringListToString(zone.UniqueMonsters),
                    critters: convertStringListToString(zone.Critters),
                    critter_spwn_pct: convertNumberListToString(
                        zone.CritterSpawnPercentages
                    ),
                    mon_lvls_exp: convertNumberListToString(
                        zone.MonsterLevelsExpansion
                    ),
                    name: zone.Name,
                    zone_name: zone.LevelName,
                    quest: zone.Quest ? zone.Quest : 0,
                    quest_part: zone.QuestPart ? zone.QuestPart : 0,
                    act: zone.Act ? zone.Act : 1,
                    teleport: zone.Teleport,
                    num_mon: zone.NumMon ? zone.NumMon : 0,
                    waypoint: zone.Waypoint ? zone.Waypoint : 0,
                    town: town,
                    zone_id: zone.Id,
                    is_inside: zone.IsInside ? true : false,
                })
                .then((res: number) => {
                    console.log(res);
                })
                .then(zoneTrx.commit)
                .finally(console.log("done1"));

            Object.entries(zone.levelDifficultyVariables).forEach(
                async ([, zoneDiffVars], i) => {
                    const diffStatTrx = await knex.transaction();
                    await diffStatTrx("zonelvldiffvars")
                        .insert({
                            zone_name: zoneName,
                            zone_id: zone.Id,
                            size_x: zoneDiffVars.SizeX,
                            size_y: zoneDiffVars.SizeY,
                            mon_den: zoneDiffVars.MonDen
                                ? zoneDiffVars.MonDen
                                : 0,
                            mon_u_min: zoneDiffVars.MonUMin
                                ? zoneDiffVars.MonUMin
                                : 0,
                            mon_u_max: zoneDiffVars.MonUMax
                                ? zoneDiffVars.MonUMax
                                : 0,
                            difficulty: i,
                        })
                        .then((res: number) => {
                            console.log(res);
                        })
                        .then(diffStatTrx.commit)
                        .finally(console.log("done2"));
                }
            );
        });
    } catch (err) {
        console.log(err);
    }
};

interface Experience {
    Level: number;
    Amazon: number;
    Sorceress: number;
    Necromancer: number;
    Paladin: number;
    Barbarian: number;
    Druid: number;
    Assassin: number;
    ExpRatio: number;
}

var ExperienceData = require("../../d2data/experience.json") as Record<
    string,
    Experience
>;
const seedExperience = async () => {
    try {
        let minimumCurrent = 0;
        for (let [lvl, curExp] of Object.entries(ExperienceData)) {
            const trx = await knex.transaction();
            await trx("experiences")
                .insert({
                    level: curExp.Level,
                    next_level_abs: curExp.Amazon,
                    amount_to_next: curExp.Amazon - minimumCurrent,
                })
                .then((res: number) => {
                    console.log(res);
                })
                .then(trx.commit);
            minimumCurrent = curExp.Amazon;
        }
    } catch (err) {
        console.log(err);
    }
};

const convertStringListToString = (list: any[]) => {
    return `${list.reduce((total, curr, i) => {
        return i == 0 ? `${total}\"s${curr}\"` : `${total},\"s${curr}\"`;
    }, "[")}]`;
};

const convertNumberListToString = (list: any[]) => {
    return `${list.reduce((total, curr, i) => {
        return i == 0 ? `${total}${curr}` : `${total},${curr}`;
    }, "[")}]`;
};
const seedWhatINeed = () => {
    // seedMonLvl();
    seedMonsterStats();
    // seedZoneData();
    // seedExperience();
};

seedWhatINeed();
