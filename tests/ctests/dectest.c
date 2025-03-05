#define DECIMCALC1_MAX 18
#define DECIMCALC1_PRE 6
typedef decimal(DECIMCALC1_MAX,DECIMCALC1_PRE) DECIMCALC1;

short RchNUNENJInd(short *, decimal(15), decimal(15)*, short);

int main () {
  decimal(18,6) dMontPrelPropHO;
  decimal(7,4) dTauxPrelHO;
  decimal(1,0) WS_MDDRA;
  DECIMCALC1 d_mt_manque = D_ZERO;
  pstDataOut->stEDIT_RPAY[s_idx_edit_rpay].d_rap_net_res_paya =
    (DECIMCALC1)pstDataOPG->stRPAY[s_idx_rpay_tmp].d_rap_net_res_paya;
  decimal(15,6) dSeuilOPA = 0;
  decimal(18,6) dOMC_JEBP_ORG[4];
  int TraiterTYRP(decimal(18,6));
//decimal(7,4) TraiterTaux(decimal(9,8) dTaux)
  static decimal(10, 8) hvdTXPRL;
  pAggOpar->DMtJec = (decimal(15,0))(0);
  HTRPTOJX.JEOPX = (decimal(18,6))(bet->amoWag) / 100.0d ;
  HTRPTFAR.MASSPAR=((decimal(13,2))(pool)) / 100;
  decimal(13,7) *taux;
  if (lRC == OK) *taux = ((decimal(13,7)*)contenu)[numDevise];
  if (lRC == OK) *taux = ((decimal(7)*)contenu)[numDevise];
}
