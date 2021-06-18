"use strict";
require("dotenv").config();
var CharStats = require("../../d2data/charstats.json");
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
                .then((res) => {
                console.log(res);
            })
                .then(trx.commit);
        }
    });
}
catch (err) {
    console.log(err);
}
//# sourceMappingURL=seed.js.map