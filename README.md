# The Limits of Second Best: CBAM and Korean Exports to the EU
서울대학교 PPE 학사학위논문(2026) 실증분석 재현 코드

- cbam_code_v2.R : 데이터 수집, 품목 분류, 이중차분 회귀, 가짜 처치 검정, 표·그림 생성
- cbam_classification_*.xlsx : 보수적/완화적/절충적 분류기준별 CN-HS 매핑표

데이터: 관세청 품목별 국가별 수출입실적 Open API (data.go.kr)
실행: data.go.kr 인증키를 환경변수 DATA_GO_KR_KEY로 설정한 뒤 스크립트를 위에서부터 실행
