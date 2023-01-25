const SERVER_URL = "http://localhost:3050/taxkbapi";
// const SERVER_URL = "https://legalmachinelab.unibo.it/logicalenglish";
const MY_TOKEN = "myToken123";

import axios from 'axios';
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

async function loadFile(ServerFile){
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "load", 
        file: ServerFile
        }, axiosConfig)).data;
}

async function loadString(LE){
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "load", 
        le: LE
        }, axiosConfig)).data;
}

// TODO: variant using template argument names as object fields/keys
async function loadFactsAndQuery(sessionModule,facts,goal='true',vars=[]){
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "loadFactsAndQuery", 
        sessionModule:sessionModule,
        facts: facts, goal:goal, vars:vars
        }, axiosConfig)).data;
}

// felice è padre di giuseppe.
// tatiana è madre di giuseppe.
// felice ha la cittadinanza italiana.
// tatiana ha la cittadinanza italiana.

// scenario filippo è:
// filippo è nato in italia.


async function main(filename){
    if (!filename)
        filename = '../moreExamples/cittadinanza_italiana.le'
    console.log("\nNow loading LE from a server file: " + filename);
    var result2 = await loadFile(filename);

    // console.log("Overall result 2:"); console.log(JSON.stringify(result2,null,4));

    var result4 = await loadFactsAndQuery(result2.sessionModule, [
            "è_padre_di('Felice','Giuseppe')", 
            "ha_la_cittadinanza_italiana('Felice')",
            "è_nato_in_italia('Filippo')"
        ],
        "ha_la_cittadinanza_italiana(Person)",
        ["Person"]
    );
    console.log("Overall result 4:");
    console.log(JSON.stringify(result4,null,4).replace(/\\n\s+/g, " "));

}

if (length(argv) > 1) {
    console.error("Usage: node " + argv[1] + " [filename]")
    process.exit(1)
}
main(argv);
