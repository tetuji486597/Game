// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var CS17SetupGame$Game = require("./CS17SetupGame.bs.js");

function transpose(mat) {
  if (!mat) {
    return Pervasives.failwith("A matrix cannot be 0 - dimensional .");
  }
  var match = mat.hd;
  if (!match) {
    return Pervasives.failwith("A matrix cannot be 0 - dimensional .");
  }
  if (!match.tl) {
    return {
            hd: List.map(List.hd, mat),
            tl: /* [] */0
          };
  }
  var firstCol = List.map(List.hd, mat);
  var restMatrix = List.map(List.tl, mat);
  return {
          hd: firstCol,
          tl: transpose(restMatrix)
        };
}

function vertFlip(matrix) {
  return List.map(List.rev, matrix);
}

function horzFlip(matrix) {
  if (matrix) {
    return Pervasives.$at(horzFlip(matrix.tl), {
                hd: matrix.hd,
                tl: /* [] */0
              });
  } else {
    return /* [] */0;
  }
}

function checkFourInARow(_c) {
  while(true) {
    var c = _c;
    if (!c) {
      return false;
    }
    var match = c.tl;
    if (!match) {
      return false;
    }
    var match$1 = match.tl;
    var t2 = match.hd;
    if (!match$1) {
      return false;
    }
    var match$2 = match$1.tl;
    var t3 = match$1.hd;
    if (!match$2) {
      return false;
    }
    var t4 = match$2.hd;
    if (c.hd === t2 && t2 === t3 && t3 === t4 && t4 !== 0) {
      return true;
    }
    _c = {
      hd: t2,
      tl: {
        hd: t3,
        tl: {
          hd: t4,
          tl: match$2.tl
        }
      }
    };
    continue ;
  };
}

function checkVerticalWin(_b) {
  while(true) {
    var b = _b;
    if (!b) {
      return false;
    }
    if (checkFourInARow(b.hd)) {
      return true;
    }
    _b = b.tl;
    continue ;
  };
}

function checkHorizontalWin(b) {
  return checkVerticalWin(transpose(b));
}

function mainDiagonal(matrix) {
  return List.mapi((function (rowIndex, row) {
                return List.nth(row, rowIndex);
              }), matrix);
}

function upper_NWSE_Diagonals(b) {
  if (b) {
    return {
            hd: mainDiagonal(b),
            tl: upper_NWSE_Diagonals(b.tl)
          };
  } else {
    return /* [] */0;
  }
}

function allDiagonals(mat) {
  return Pervasives.$at(upper_NWSE_Diagonals(mat), Pervasives.$at(upper_NWSE_Diagonals(transpose(mat)), Pervasives.$at(upper_NWSE_Diagonals(horzFlip(mat)), upper_NWSE_Diagonals(horzFlip(transpose(mat))))));
}

function checkDiagonalWin(b) {
  return checkVerticalWin(allDiagonals(b));
}

var exampleBoard = {
  hd: {
    hd: 0,
    tl: {
      hd: 0,
      tl: {
        hd: 2,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 1,
              tl: /* [] */0
            }
          }
        }
      }
    }
  },
  tl: {
    hd: {
      hd: 0,
      tl: {
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 1,
            tl: {
              hd: 2,
              tl: {
                hd: 1,
                tl: /* [] */0
              }
            }
          }
        }
      }
    },
    tl: {
      hd: {
        hd: 0,
        tl: {
          hd: 0,
          tl: {
            hd: 2,
            tl: {
              hd: 2,
              tl: {
                hd: 1,
                tl: {
                  hd: 1,
                  tl: /* [] */0
                }
              }
            }
          }
        }
      },
      tl: {
        hd: {
          hd: 0,
          tl: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 2,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        },
        tl: {
          hd: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 1,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 1,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          },
          tl: {
            hd: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 0,
                  tl: {
                    hd: 0,
                    tl: {
                      hd: 1,
                      tl: {
                        hd: 1,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            },
            tl: {
              hd: {
                hd: 0,
                tl: {
                  hd: 0,
                  tl: {
                    hd: 0,
                    tl: {
                      hd: 2,
                      tl: {
                        hd: 2,
                        tl: {
                          hd: 1,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              },
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
};

var exampleBoard2 = {
  hd: {
    hd: 0,
    tl: {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 1,
              tl: /* [] */0
            }
          }
        }
      }
    }
  },
  tl: {
    hd: {
      hd: 0,
      tl: {
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 1,
            tl: {
              hd: 2,
              tl: {
                hd: 1,
                tl: /* [] */0
              }
            }
          }
        }
      }
    },
    tl: {
      hd: {
        hd: 0,
        tl: {
          hd: 0,
          tl: {
            hd: 2,
            tl: {
              hd: 1,
              tl: {
                hd: 1,
                tl: {
                  hd: 1,
                  tl: /* [] */0
                }
              }
            }
          }
        }
      },
      tl: {
        hd: {
          hd: 0,
          tl: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 1,
                tl: {
                  hd: 1,
                  tl: {
                    hd: 1,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        },
        tl: {
          hd: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 1,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 2,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          },
          tl: {
            hd: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 0,
                  tl: {
                    hd: 0,
                    tl: {
                      hd: 1,
                      tl: {
                        hd: 1,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            },
            tl: {
              hd: {
                hd: 0,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 2,
                      tl: {
                        hd: 2,
                        tl: {
                          hd: 1,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              },
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
};

var exampleBoard3 = {
  hd: {
    hd: 0,
    tl: {
      hd: 0,
      tl: {
        hd: 2,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 1,
              tl: /* [] */0
            }
          }
        }
      }
    }
  },
  tl: {
    hd: {
      hd: 0,
      tl: {
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 1,
            tl: {
              hd: 2,
              tl: {
                hd: 1,
                tl: /* [] */0
              }
            }
          }
        }
      }
    },
    tl: {
      hd: {
        hd: 0,
        tl: {
          hd: 0,
          tl: {
            hd: 2,
            tl: {
              hd: 2,
              tl: {
                hd: 1,
                tl: {
                  hd: 1,
                  tl: /* [] */0
                }
              }
            }
          }
        }
      },
      tl: {
        hd: {
          hd: 0,
          tl: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 1,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        },
        tl: {
          hd: {
            hd: 0,
            tl: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 1,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          },
          tl: {
            hd: {
              hd: 0,
              tl: {
                hd: 0,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 1,
                      tl: {
                        hd: 1,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            },
            tl: {
              hd: {
                hd: 0,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 2,
                      tl: {
                        hd: 2,
                        tl: {
                          hd: 1,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              },
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
};

CS17SetupGame$Game.checkExpect(checkVerticalWin(transpose(exampleBoard)), false, "no horizontal win");

CS17SetupGame$Game.checkExpect(checkVerticalWin(transpose(exampleBoard2)), true, "horizontal win in bottom row");

CS17SetupGame$Game.checkExpect(checkVerticalWin(allDiagonals(exampleBoard)), false, "no diagonal");

CS17SetupGame$Game.checkExpect(checkVerticalWin(allDiagonals(exampleBoard3)), true, "2 diagonal");

CS17SetupGame$Game.checkExpect(checkVerticalWin(exampleBoard), false, "no four in a row");

CS17SetupGame$Game.checkExpect(checkVerticalWin(exampleBoard2), true, "four in a row in last");

CS17SetupGame$Game.checkExpect(checkFourInARow({
          hd: 0,
          tl: {
            hd: 0,
            tl: {
              hd: 1,
              tl: {
                hd: 1,
                tl: {
                  hd: 1,
                  tl: {
                    hd: 1,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }), true, "there is four in a row");

exports.transpose = transpose;
exports.vertFlip = vertFlip;
exports.horzFlip = horzFlip;
exports.checkFourInARow = checkFourInARow;
exports.checkVerticalWin = checkVerticalWin;
exports.checkHorizontalWin = checkHorizontalWin;
exports.mainDiagonal = mainDiagonal;
exports.upper_NWSE_Diagonals = upper_NWSE_Diagonals;
exports.allDiagonals = allDiagonals;
exports.checkDiagonalWin = checkDiagonalWin;
exports.exampleBoard = exampleBoard;
exports.exampleBoard2 = exampleBoard2;
exports.exampleBoard3 = exampleBoard3;
/*  Not a pure module */