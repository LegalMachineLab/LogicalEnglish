import requests
import json
import argparse
import importlib
from rich.tree import Tree
from rich import print as rprint

def setup_argparse():
    parser = argparse.ArgumentParser("Query an LE server")
    parser.add_argument("--server", help="LE server to query", default="http://localhost:3050/taxkbapi")
    parser.add_argument("--token", help="Token to use", default="myToken123")
    parser.add_argument("--module", help="Module to load", default="../moreExamples/cittadinanza_italiana.le")
    parser.add_argument("--facts", help="Facts to load", default=["è_padre_di('Felice','Giuseppe')", "ha_la_cittadinanza_italiana('Felice')", "è_nato_in_italia('Filippo')"])
    parser.add_argument("--goal", help="Goal to query", default="ha_la_cittadinanza_italiana(Person)")
    parser.add_argument("--vars", help="Variables to return", default=["Person"])
    parser.add_argument("--rich", help="Use rich to print results", action="store_true", default=False)
    parser.add_argument("--poor", help="Do not use rich to print results", action="store_true", default=False)
    return parser

def query_server(data) -> json:
    session = requests.Session()
    tmp = session.post(
        data.server,
        json={
            "token": data.token,
            "operation": "load",
            "file": data.module,
        },
        # headers={"Access-Control-Allow-Origin": "*"},
    )

    if tmp.status_code != 200:
        print(tmp.json())
        exit()

    response = session.post(
        data.server,
        json={
            "token": data.token,
            "operation": "loadFactsAndQuery",
            "sessionModule": tmp.json()["sessionModule"],
            "facts": data.facts,
            "goal": data.goal,
            "vars": data.vars,
        },
        # headers={"Access-Control-Allow-Origin": "*"},
    )

    return response.json()


def print_explanation(explanation, tab):
    literal = explanation["literal"].split("(")
    rprint(tab*" ", ("[red]" if explanation["type"] == "failure" else "[green]") + literal[0], "(" + literal[1])
    for i in explanation["children"]:
        print_explanation(i, tab+4)


def print_answer(answer):
    print(answer["bindings"])
    print_explanation(answer["explanation"][0], 0)


def build_tree(tree, explanation):
    # literal = explanation["literal"].split("(")
    # new_literal = "[red]" if explanation["type"] == "failure" else "[green]" + literal[0] + "(" + literal[1]
    new_literal = ("[red]" if explanation["type"] == "failure" else "[green]") + explanation["literal"]
    new_tree = tree.add(new_literal)
    for i in explanation["children"]:
        build_tree(new_tree, i)
    return tree


if __name__ == "__main__":
    parser = setup_argparse()
    args = parser.parse_args()
    answer = query_server(args)

    if args.rich and importlib.util.find_spec("rich") is not None:
    # Rich print
        for i in answer["answers"]:
            tree = Tree(str(i["bindings"]))
            build_tree(tree, i["explanation"][0])
            rprint(tree)

    else:
    # Non rich print
        for i in answer["answers"]:
            print("Answer", answer["answers"].index(i))
            print_answer(i)

    # print(json.dumps(answer, indent=4, ensure_ascii=False))
    # .encode("utf8").decode()
