#include <iostream>
#include "HepMC/PythiaWrapper.h"
#include "HepMC/IO_HEPEVT.h"
#include "HepMC/IO_GenEvent.h"
#include "HepMC/GenEvent.h"
using namespace std;

static HepMC::IO_HEPEVT hepevtio;
static HepMC::IO_GenEvent ascii_out("output.hepmc", std::ios::out);
static int evtcount=0;

extern "C"
{
  
  void writeevent_()
  {
    evtcount++;
    call_pyhepc(1);
    HepMC::GenEvent* evt = hepevtio.read_next_event();
    evt->use_units(HepMC::Units::GEV, HepMC::Units::MM);
    if (evtcount % 1000 == 0) {
      cout << "writing event: " << evtcount << endl;
    }
    ascii_out << evt;
    delete evt;
  }
}
