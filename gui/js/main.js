async function updateHTML(gameJSON) {
  const status = $("#status");
  status.removeClass("stopped");
  status.removeClass("started");

  if (gameJSON.gameStarted) {
    status.addClass("playing");
    document.querySelector("#status").innerHTML = "Started";
  } else {
    status.addClass("stopped");
    document.querySelector("#status").innerHTML = "Stopped";
  }

  // await movePiece(gameJSON.players[0].position);
  document.querySelector("#p1Name").innerHTML = gameJSON.players[0].name;
  document.querySelector("#p1Bal").innerHTML =
    "$" + gameJSON.players[0].balance;

  movePiece(gameJSON.players[0].position);
}

async function movePiece(pos) {
  const pixel_pos = positionsTransform[pos];
  console.log("pixel", `translate(${pixel_pos.x}, ${pixel_pos.y})`);
  $("#p1-piece").css({
    transform: `translate(${pixel_pos.x}px, ${pixel_pos.y}px)`,
  });
}

async function readJSON(file) {
  var rawFile = new XMLHttpRequest();
  rawFile.overrideMimeType("application/json");

  rawFile.open("GET", file, true);

  rawFile.onreadystatechange = async function () {
    if (rawFile.readyState === 4 && rawFile.status == "200") {
      var gameInfo = JSON.parse(rawFile.responseText);
      console.log(gameInfo);
      await updateHTML(gameInfo);
    }
  };

  rawFile.send(null);
}

let count = 0;
const refreshInterval = setInterval(async () => {
  await readJSON("./data/game.json");
  console.log("updated!");
}, 500);

const positionsTransform = {
  0: {
    x: -260,
    y: 565,
  },
  1: {
    x: -260,
    y: 480,
  },
  2: {
    x: -260,
    y: 430,
  },
  3: {
    x: -260,
    y: 380,
  },
  4: {
    x: -260,
    y: 330,
  },
  5: {
    x: -260,
    y: 280,
  },
  6: {
    x: -260,
    y: 230,
  },
  7: {
    x: -260,
    y: 180,
  },
  8: {
    x: -260,
    y: 130,
  },
  9: {
    x: -260,
    y: 90,
  },
  10: {
    x: -250,
    y: 30,
  },
  11: {
    x: -200,
    y: 30,
  },
  12: {
    x: -150,
    y: 30,
  },
  13: {
    x: -100,
    y: 30,
  },
  14: {
    x: -50,
    y: 30,
  },
  15: {
    x: 0,
    y: 30,
  },
  16: {
    x: 50,
    y: 30,
  },
  17: {
    x: 100,
    y: 30,
  },
  18: {
    x: 150,
    y: 30,
  },
  19: {
    x: 200,
    y: 30,
  },
  20: {
    x: 250,
    y: 30,
  },
  21: {
    x: 250,
    y: 90,
  },
  22: {
    x: 250,
    y: 140,
  },
  23: {
    x: 250,
    y: 190,
  },
  24: {
    x: 250,
    y: 240,
  },
  25: {
    x: 250,
    y: 290,
  },
  26: {
    x: 250,
    y: 340,
  },
  27: {
    x: 250,
    y: 390,
  },
  28: {
    x: 250,
    y: 440,
  },
  29: {
    x: 250,
    y: 490,
  },
  30: {
    x: 250,
    y: 550,
  },
  31: {
    x: 195,
    y: 550,
  },
  32: {
    x: 145,
    y: 550,
  },
  33: {
    x: 100,
    y: 550,
  },
  34: {
    x: 50,
    y: 550,
  },
  35: {
    x: 0,
    y: 550,
  },
  36: {
    x: -50,
    y: 550,
  },
  37: {
    x: -100,
    y: 550,
  },
  38: {
    x: -150,
    y: 550,
  },
  39: {
    x: -200,
    y: 550,
  },
};
